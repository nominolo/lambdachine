{-# LANGUAGE BangPatterns, PatternGuards, GADTs #-}
module Lambdachine.Interp.Exec
  ( module Lambdachine.Interp.Exec,
    module Lambdachine.Interp.Types
  )
where

import Lambdachine.Interp.Types
import Lambdachine.Interp.Trace
import Lambdachine.Grin.Bytecode
import Lambdachine.Id
import Lambdachine.Grin.RegAlloc ( LinearCode(..) )
import Lambdachine.Utils
import Lambdachine.Builtin

import Control.Monad ( forM_, when )
import Data.Array.IO
import Data.Maybe ( fromMaybe )
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S

-- -------------------------------------------------------------------


addFrame :: Int -> ArgStack -> IO ArgStack
addFrame frame_size (ArgStack base _top args) = do
  (_, max_top) <- getBounds args
  let !top' = base + frame_size
  if top' >= max_top then
    error "Stack Overflow"
   else
     return (ArgStack base top' args)

stackFrameSize :: Int
stackFrameSize = 3  -- prevFrame, savedPC, curNode

-- | Allocate a stack frame.
--
-- Stack frame layout.  Before call:
--
-- @
--  +-----------+ <- base
--  | R0 = 42   |
--  +-----------+ <- base + sizeof(Word)
--  | R1 = 23   |
--  +-----------+
--  | R2 = &f   |
--  +-----------+ <- top
-- @
--
-- After `call R2(R1,R0):
--
-- @
--  +-----------+ <-.  (old base)
--  |     -     |   |
--  +-----------+   |
--  |     -     |   |
--  +-----------+   |
--  |     -     |   |
--  +-----------+   |
--  | oldFrame *----'
--  +-----------+
--  |  savedPC  |   return address
--  +-----------+
--  |    &f     |   current closure
--  +-----------+ <- base
--  | R0 = 23   |
--  +-----------+
--  | R1 = 42   |
--  +-----------+
--  |           |
--      ....
--  |           |
--  +-----------+ <- top = base + f->framesize
-- @
allocFrame :: ArgStack -> Int -> Val -> [Val] -> Int -> IO ArgStack
allocFrame (ArgStack base top args) pc fun params framesize = do
  (_, max_top) <- getBounds args
  let !top' = top + stackFrameSize + framesize
  if top' >= max_top then
    error "Stack Overflow"
   else do
--     pprint $ text "... Setting up stackframe of size"
--              <+> ppr (stackFrameSize + framesize)
    let !base' = top + 3
    writeArray args top (ValI (fromIntegral base))  -- prevFrame
    writeArray args (top + 1) (ValI (fromIntegral pc))  -- savedPC
    writeArray args (top + 2) fun  -- curNode
    forM_ (zip [base' ..] params) $ \(i, v) ->
      writeArray args i v
    return $ ArgStack base' top' args

initArgStack :: Id -> ArgStack -> IO ArgStack
initArgStack entry (ArgStack _base _top args) = do
  writeArray args 0 (SLoc initCodeId)
  writeArray args 1 (SLoc entry)
  return (ArgStack 1 2 args)

adjustFramesize :: ArgStack -> Int -> IO ArgStack
adjustFramesize (ArgStack base top args) new_framesize =
  let !top' = base + new_framesize in
  if top' <= top then do
    --pprint $ text "No stack growing" <+> ppr (top, top', new_framesize)
    return (ArgStack base top' args)
   else do
     (_, max_top) <- getBounds args
     if top' >= max_top then
       error "Stack Overflow"
      else
       return (ArgStack base top' args)

popFrameRet :: InfoTables -> Heap -> ArgStack -> Val
            -> IO (ArgStack, InfoTable, Int)
popFrameRet env heap (ArgStack base top args) result = do
  let !prev_top = base - 3
  ValI prev_base <- readArray args prev_top
  ValI old_pc <- readArray args (prev_top + 1)
  let base' = fromIntegral prev_base
  let pc' = fromIntegral old_pc
  old_node <- readArray args (base' - 1)
  let Closure itbl_ptr _ = lookupClosure heap old_node
      bco = lookupInfoTable env itbl_ptr
--   pprint $ text "Trying to return to:" <+>
--              align (ppr pc' <+> ppr itbl_ptr <+> ppr bco)
  case fc_code (itblCode bco) V.! (pc' - 1) of
    Lst (Call (Just (BcReg dst, _, _)) _ _) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    Lst (Eval _ _ (BcReg dst)) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    other ->
      error $ "Not returning to Call or Eval: " ++ pretty other


mkDummyFun :: Int -> [FinalIns] -> BytecodeObject' FinalCode
mkDummyFun framesize code0 =
  BcObject (BcoFun framesize) code [] [] 0
 where
  code = FinalCode
    { fc_framesize = framesize
    , fc_code = V.fromList code0
    }

{-
pre_post :: [Val] -> [FinalIns] -> [Maybe Val] -> IO Bool
pre_post pre code post = do
  let frame_size = (length pre)
  args <- addFrame frame_size =<< mkArgStack
  forM_ (zip [0..] pre) $ \(n, val) -> writeReg args n val
  let (heap, env) = initHeap M.empty
  let bco = mkDummyFun frame_size code
  (heap', args') <- interpSteps (V.length (fc_code (bcoCode bco))) env heap bco 0 args []
  arg_elems <- take frame_size `fmap` getElems (argVals args')
  pprint (arg_elems, heap')
  return $ and $ zipWith verify_arg post arg_elems
 where
   verify_arg Nothing _ = True
   verify_arg (Just v) v' = v == v'
-}
interpSteps :: Int -> VMState -> IO VMState
interpSteps steps vm0 = do
  let PC itbl pc = vm_pc vm0
  go steps vm0 (vm_heap vm0) itbl pc (vm_stack vm0)
 where
   go n vm heap itbl pc stack | n <= 0 = do
     putStrLn "STOPPING (ran out of steps)"
     return (vm{ vm_heap = heap,
                 vm_stack = stack,
                 vm_pc = PC itbl pc })
   go n vm heap itbl pc stack =
     interp1 vm heap itbl pc stack (go (n - 1)) stop
   stop vm heap itbl pc stack = do
     putStrLn "FINISHED"
     return (vm{ vm_heap = heap,
                 vm_stack = stack,
                 vm_pc = PC itbl pc })

decr_hot :: VMState -> InfoTableId -> (Bool, VMState)
decr_hot vm itbl_id =
  case M.lookup itbl_id (vm_hotcounts vm) of
    Nothing ->
      (False, vm{ vm_hotcounts =
                    M.insert itbl_id (vm_hotfunc vm) (vm_hotcounts vm) })
    Just c
      | c > 1 -> 
        let !c' = c - 1 in
        (False, vm{ vm_hotcounts =
                      M.insert itbl_id c' (vm_hotcounts vm) })
      | otherwise ->
        (True,
         vm{ vm_hotcounts = M.insert itbl_id 0 (vm_hotcounts vm) })

-- | Interpret current instruction and call continuation with modified
-- interpreter state.
interp1 :: VMState -> Heap -> InfoTable -> Int -> ArgStack
        -> (VMState -> Heap -> InfoTable -> Int -> ArgStack -> IO a)
        -> (VMState -> Heap -> InfoTable -> Int -> ArgStack -> IO a)
        -> IO a
interp1 vm0@VMState{ vm_env = env } heap
        bco@CodeInfoTable{ itblCode = code } pc args k kstop = do
  let inst = fc_code code V.! pc
  stack <- ppArgStack args heap
  
  pprint $ text ">>>" <+> indent (stackDepth args)
                            (ppr pc <> colon <+> ppr inst {- $+$
                             text "..." <+> stack -})
  
  vm <- case vm_mode vm0 of
          Interp -> return vm0
          Trace root rs -> do
            --               pprint $ text "Recording"
            keep_going <- record1 root env heap args bco pc inst rs
            if keep_going then return vm0
             else do
               -- TODO: Finish trace and replace function.
               finaliseTrace rs env heap
               let vm' = vm0{ vm_mode = Interp }
               error "FIXME: stopping for now."
               kstop vm' heap bco pc args
               return vm'
               error "FIXME: stopping for now."

  case inst of
    Mid (Assign (BcReg dest) rhs) -> interp_rhs vm dest rhs
    Mid (Store base offs src) -> store vm base offs src
    Lst (Call (Just (dst, _, _)) fun vars) -> call vm dst fun vars
    Lst (Call Nothing fun vars) -> tailcall vm fun vars
    Lst (Ret1 (BcReg x)) -> do
      rslt <- readReg args x
      (args', bco', pc') <- popFrameRet env heap args rslt
      k vm heap bco' pc' args'
    Lst (Goto pc') -> k vm heap bco pc' args
    Lst (Case CaseOnTag (BcReg x) alts) -> case_branch vm x alts
    Lst (Eval pc' _ (BcReg reg)) -> assert (pc' == pc + 1) $ eval vm reg
    Lst Update -> update vm
    Lst Stop -> kstop vm heap bco pc args
 where
{-   (is_hot, vm) | pc == 0 && not (isRecording vm0) 
                            = decr_hot vm0 (itblId bco)
                | otherwise = (False, vm0) -}
   interp_rhs vm dst (Move (BcReg src)) = do
     pprint =<< ppArgStack args heap
     old_src <- readReg args src
     writeReg args dst old_src
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (BinOp op Int32Ty (BcReg r1) (BcReg r2)) = do
     ValI v1 <- readReg args r1
     ValI v2 <- readReg args r2
     writeReg args dst (bin_op_int32ty op v1 v2)
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (Load (LoadLit c)) = do
     writeReg args dst (constToVal c)
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (Load (LoadGlobal x)) = do
     writeReg args dst (SLoc x)
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (Load LoadBlackhole) = do
     writeReg args dst (SLoc blackholeDataConId)
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (Load (LoadClosureVar i)) = do
     node_ptr <- readReg args (-1)
     let Closure _ free_vars = lookupClosure heap node_ptr
     writeReg args dst (free_vars V.! (i - 1))
     k vm heap bco (pc + 1) args
   interp_rhs vm dst (Alloc (BcReg con_info) vars) = do
     SLoc dcon <- readReg args con_info
     alloc vm dst dcon =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs vm dst (AllocAp vars) = do
     let apsize = ValI (fromIntegral (length vars))
     alloc vm dst apDataConId . (apsize:) =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs vm dst (Fetch (BcReg src) offs) = do
     ptr <- readReg args src
     let cl = lookupClosure heap ptr
     writeReg args dst (getField cl offs)
     k vm heap bco (pc + 1) args

   alloc vm dst dcon vals = do
     let cl = Closure (ItblId dcon) (V.fromList vals)
     let (heap', ptr) = allocClosure heap cl
     writeReg args dst ptr
     k vm heap' bco (pc + 1) args

   store vm (BcReg dst) offs (BcReg src) = do
     ptr <- readReg args dst
     val <- readReg args src
     -- write barrier goes somewhere below
     let cl = lookupClosure heap ptr
         cl' = setField cl offs val
         heap' = updateClosure heap ptr cl'
     k vm heap' bco (pc + 1) args

   call vm (BcReg _dst) (BcReg fun) vars = do
     fn <- readReg args fun
     let num_args = length vars
     let Closure itbl_id _ = lookupClosure heap fn
         itbl' = lookupInfoTable env itbl_id
     case itblClosureType itbl' of
       CtFun arity
         | arity == num_args -> do
            vals <- mapM (\(BcReg r) -> readReg args r) vars
            args' <- allocFrame args (pc + 1) fn vals (fc_framesize (itblCode itbl'))
            pprint $ text "***" <+> indent (stackDepth args') (text "Entering:" <+> ppr fun)
            let (is_hot, vm') | isRecording vm = (False, vm)
                              | otherwise = decr_hot vm itbl_id
            when is_hot $ pprint $ text "HOT:" <+> ppr (itblId bco)
            vm'' <- if not is_hot then return vm'
                     else do
                       let (ArgStack base top _) = args
                       rs <- mkRecordState (top - base)
                       return vm'{ vm_mode = Trace itbl_id rs }
            k vm'' heap itbl' 0 args'

   tailcall vm (BcReg fun) vars = do
     fn <- readReg args fun
     let num_args = length vars
     let Closure itbl_id _ = lookupClosure heap fn
         itbl' = lookupInfoTable env itbl_id
     case itblClosureType itbl' of
       CtFun arity
         | arity == num_args -> do
            vals <- mapM (\(BcReg r) -> readReg args r) vars
            forM_ (zip [0..] vals) $ \(i, v) -> writeReg args i v
            writeReg args (-1) fn
            args' <- adjustFramesize args (fc_framesize (itblCode itbl'))
            pprint $ text (replicate (stackDepth args' + 4) '*') <+> (text "Entering:" <+> ppr fn)
            let (is_hot, vm') | isRecording vm = (False, vm)
                              | otherwise = decr_hot vm itbl_id
            when is_hot $ pprint $ text "HOT:" <+> ppr (itblId bco)
            vm'' <- if not is_hot then return vm'
                     else do
                       let (ArgStack base top _) = args
                       rs <- mkRecordState (top - base)
                       return vm'{ vm_mode = Trace itbl_id rs }
            k vm'' heap itbl' 0 args'

   case_branch vm reg alts = do
     addr <- readReg args reg
     let Closure itbl _ = lookupClosure heap addr
     let bco' = lookupInfoTable env itbl
     case bco' of
       ConstrInfoTable{ itblTag = tag } -> do
         let pc' = find_alt tag alts
         --pprint $ text "Found tag:" <+> ppr tag <+> text "->" <+> ppr pc'
         k vm heap bco pc' args
    where
      find_alt tag ((DefaultTag, _, offs) : alts') = find_alt' tag alts' offs
      find_alt tag alts' = find_alt' tag alts' (error "Unmatched pattern")

      find_alt' tag [] dflt = dflt
      find_alt' tag ((Tag tag', _, dst) : alts') dflt
        | tag == tag' = dst
        | otherwise   = find_alt' tag alts' dflt

   eval vm reg = do
     node_ptr <- readReg args reg
     let Closure itbl_id _ = lookupClosure heap node_ptr
     case lookupInfoTable env itbl_id of
       ConstrInfoTable{} -> k vm heap bco (pc + 1) args
       bco'@CodeInfoTable{ itblClosureType = cl_ty }
         | CtFun _ <- cl_ty -> k vm heap bco (pc + 1) args
         | otherwise -> eval_it vm bco' node_ptr

   eval_it vm bco'@CodeInfoTable{ itblCode = code } node_ptr = do
     (args', pc') <- pushUpdateFrame args (pc+1) node_ptr
     -- like call
     args'' <- allocFrame args' pc' node_ptr [] (fc_framesize code)
     pprint $ text (replicate (stackDepth args'' + 4) '*') <+>
              text "Eval-Entering:" <+>
              ppr (cl_itbl (lookupClosure heap node_ptr))
     k vm heap bco' 0 args''

   update vm = do
     -- Semantics: *R(0) = *R(1)  // or create indirection
     --            return R(1)  -or-  R(0) = R(1); return R(0)
     old_ptr <- readReg args 0
     new_ptr <- readReg args 1
     let new_cl = lookupClosure heap new_ptr
         heap' = updateClosure heap old_ptr new_cl
     (args', bco', pc') <- popFrameRet env heap' args new_ptr
     k vm heap' bco' pc' args'


   if' True  = SLoc trueDataConId
   if' False = SLoc falseDataConId
   bin_op_int32ty OpAdd v1 v2 = ValI $ v1 + v2
   bin_op_int32ty OpSub v1 v2 = ValI $ v1 - v2
   bin_op_int32ty OpMul v1 v2 = ValI $ v1 * v2
   bin_op_int32ty OpDiv v1 v2 = ValI $ v1 `div` v2
   bin_op_int32ty CmpGt v1 v2 = if' (v1 > v2)
   bin_op_int32ty CmpLe v1 v2 = if' (v1 <= v2)
   bin_op_int32ty CmpGe v1 v2 = if' (v1 >= v2)
   bin_op_int32ty CmpLt v1 v2 = if' (v1 < v2)
   bin_op_int32ty CmpEq v1 v2 = if' (v1 == v2)
   bin_op_int32ty CmpNe v1 v2 = if' (v1 /= v2)

interp1 _ _ bco _ _ _ _ =
  error $ "Trying to interpret: " ++ pretty bco

-- | Push an update frame.  Currently looks like this:
--
-- @
--                  ^
--  +-----------+   |
--  | oldFrame *----'
--  +-----------+
--  |  savedPC  | points after the EVAL
--  +-----------+
--  |    &f     | <- Node = the magical updateClosure
--  +-----------+ <- base
--  | R0        | <- Pointer to the node to be updated
--  +-----------+
--  | R1        | <- Used to receive the result from evaluation.
-- @
--
pushUpdateFrame :: ArgStack -> Int -> Val -> IO (ArgStack, Int)
pushUpdateFrame args pc ptr = do
  args' <- allocFrame args pc (SLoc updateId) [ptr] 2
  return (args', 1)

-- | Info table and bytecode referenced by the update frame.
--
-- If a closure needs an update the code is forced to return to this
-- \"function\" (by @EVAL@).  The bytecode currently looks as follows:
--
-- @
--   CALL R(1), <dummy>  ; force result to be written into r1
--   UPDATE              ; magical.  See below.
-- @
--
-- The @UPDATE@ instruction is currently rather complex.  It performs
-- the following steps:
--
--   - Redirect the object pointed to by @R(0)@ to the object pointed
--     to by @R(1)@.  This can be done by overwriting things in memory
--     or creating an indirection.
--
--   - Copy the value of @R(1)@ into R(0)@.  This is to make sure
--     that the result of an @EVAL@ never points to an indirection.
--
--   - Return with @R(0)@ as result.
--
updateBCO :: BCO
updateBCO =
  BcObject
    { bcoType = BcoFun 1
    , bcoConstants = []
    , bcoFreeVars = 0
    , bcoGlobalRefs = []
    , bcoCode = FinalCode
                  { fc_framesize = 2
                  , fc_code = V.fromList $ the_code } }
 where
   the_code =
     [Lst (Call (Just (BcReg 1, 1, S.fromList [BcReg 0, BcReg 1])) (BcReg 1) [])
     ,Lst Update]

updateId :: Id
updateId = updateItblId

builtInEnv :: FinalBCOs
builtInEnv = M.fromList
  [(falseItblId, BcConInfo 1)
  ,(trueItblId, BcConInfo 2)
  ,(falseDataConId, BcoCon Con falseItblId [])
  ,(trueDataConId, BcoCon Con trueItblId [])
  ,(updateId, updateBCO)
  ,(initCodeId, initCodeBCO)]

initCodeBCO :: BCO
--initCodeBCO :: InfoTable
initCodeBCO =
  -- eval r0
  -- stop
  BcObject
    { bcoType = BcoFun 1
    , bcoConstants = []
    , bcoFreeVars = 0
    , bcoGlobalRefs = []
    , bcoCode = FinalCode
                  { fc_framesize = 1
                  , fc_code = V.fromList $ the_code } }
 where
   the_code =
     [Lst (Eval 1 mempty (BcReg 0))
     ,Lst Stop]

initVM :: FinalBCOs -> String -> IO VMState
initVM bcos0 entry_name = do
  let entry:_ = [ f | f <- M.keys bcos0, show f == entry_name ]
  args <- initArgStack entry =<< mkArgStack
  let bcos = M.union builtInEnv bcos0
  let (itbls, heap) = loadBCOs bcos
  let
      Just itbl = M.lookup (mkItblId initCodeId) itbls
      pc = 0
  return VMState{ vm_env = itbls
                , vm_heap = heap
                , vm_stack = args
                , vm_pc = PC itbl pc
                , vm_mode = Interp
                , vm_hotcounts = M.empty
                , vm_hotfunc = 3 }

ppVM :: VMState -> IO PDoc
ppVM vm = ppArgStack (vm_stack vm) (vm_heap vm)

test_insts2 :: FinalBCOs -> IO ()
test_insts2 bcos0 = do
  vm <- initVM bcos0 "test"
  let steps = 1000
  vm' <- interpSteps steps vm
  pprint =<< ppVM vm'
{-
  
  let entry:_ = [ f | f <- M.keys bcos0, show f == "test" ]
  args <- initArgStack entry =<< mkArgStack
  let bcos = M.union builtInEnv bcos0
  let (itbls, heap) = loadBCOs bcos
  pprint (itbls, heap)
  let
      Just itbl = M.lookup initCodeId itbls
      pc = 0
      steps = 1000
  (heap', args') <- interpSteps steps itbls heap itbl pc args
  --arg_elems <- take frame_sizea `fmap` getElems (argVals args')
  pprint =<< ppArgStack args' heap'
  pprint heap'
-}
{-
test_insts1 :: IO ()
test_insts1 = do
  ok1 <- pre_post [ValI 2, ValI 3]
                  [Mid (Assign (BcReg 0) (Move (BcReg 1)))]
                  (map Just [ValI 3, ValI 3])
  ok2 <- pre_post [ValI 2, ValI 3]
                  [Mid (Assign (BcReg 0) (BinOp OpAdd Int32Ty (BcReg 0) (BcReg 1)))]
                  (map Just [ValI 5, ValI 3])
  ok3 <- pre_post [ValI 2]
                  [Mid (Assign (BcReg 0) (Load (LoadLit (CInt 42))))]
                  [Just (ValI 42)]
  ok4 <- pre_post [Undef, Undef]
                  [Mid (Assign (BcReg 0) (AllocAp [BcReg 0, BcReg 0]))]
                  [Nothing]
  ok5 <- pre_post [Undef, ValI 1, Undef]
                  [Mid (Assign (BcReg 0) (Load (LoadGlobal apDataConId)))
                  ,Mid (Assign (BcReg 0) (Alloc (BcReg 0) [BcReg 1, BcReg 2]))]
                  [Nothing, Nothing, Nothing]
  ok6 <- pre_post [Undef, ValI 42, Undef]
                  [Mid (Assign (BcReg 0) (Load (LoadGlobal apDataConId)))
                  ,Mid (Assign (BcReg 0) (Alloc (BcReg 0) [BcReg 1, BcReg 2]))
                  ,Mid (Assign (BcReg 0) (Fetch (BcReg 0) 1))]
                  [Just (ValI 42), Nothing, Nothing]
  ok6 <- pre_post [Undef, ValI 42, Undef]
                  [Mid (Assign (BcReg 0) (Load (LoadGlobal apDataConId)))
                  ,Mid (Assign (BcReg 0) (Alloc (BcReg 0) [BcReg 1, BcReg 2]))
                  ,Mid (Assign (BcReg 0) (Fetch (BcReg 0) 1))]
                  [Just (ValI 42), Nothing, Nothing]
  let ans = and [ok1, ok2, ok3, ok4, ok5, ok6]
  print ans
--  return ans
-}
