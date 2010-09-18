{-# LANGUAGE BangPatterns, PatternGuards, GADTs #-}
module Lambdachine.Grin.Interp where

import Lambdachine.Grin.Bytecode
import Lambdachine.Id
import Lambdachine.Grin.RegAlloc ( LinearCode(..) )
import Lambdachine.Utils
import Lambdachine.Builtin

import Control.Monad ( forM_ )
import Data.Array.IO
import Data.List  ( foldl' )
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS

data Val
  = Loc Int -- ^ Reference to a heap location.
  | SLoc Id -- ^ Reference to a static location.
  | ValI Integer
  | ValS String
  | Undef
  deriving Eq

type BCO = BytecodeObject' FinalCode

data ArgStack = ArgStack
  { argBase :: {-# UNPACK #-} !Int
  , argTop  :: {-# UNPACK #-} !Int
  , argVals :: IOArray Int Val
  }

-- -------------------------------------------------------------------

type InfoTableId = Id

data Closure = Closure InfoTableId (V.Vector Val)

data InfoTable
  = CodeInfoTable
      { itblClosureType :: ClosureType
      , itblCode :: FinalCode
      }
  | ConstrInfoTable
      { itblTag :: Int }

data ClosureType = CtFun Int | CtThunk | CtCAF

instance Pretty InfoTable where
  ppr (CodeInfoTable ct code) =
    ppr ct $+$ (indent 2 $ ppr code)
  ppr (ConstrInfoTable tag) =
    text "CONSTR" <> parens (text "tag:" <> ppr tag)

instance Pretty ClosureType where
  ppr (CtFun n) = text "FUN_" <> int n
  ppr CtThunk = text "THUNK"
  ppr CtCAF = text "CAF"

type InfoTables = M.Map InfoTableId InfoTable

lookupInfoTable :: InfoTables -> InfoTableId -> InfoTable
lookupInfoTable itbls itbl = fromMaybe err $ M.lookup itbl itbls
 where err = error $ "No info table for: " ++ pretty itbl

-- -------------------------------------------------------------------

data Heap = Heap { heapStaticArea :: M.Map Id Closure
                 , heapAllocArea  :: IM.IntMap Closure
                 , heapPtr        :: {-# UNPACK #-} !Int }

lookupClosure :: Heap -> Val -> Closure
lookupClosure heap addr@(SLoc x)
 | Just cl <- M.lookup x (heapStaticArea heap) = cl
lookupClosure heap addr@(Loc l)
 | Just cl <- IM.lookup l (heapAllocArea heap) = cl
lookupClosure _ addr = invalidHeapAddressError addr

invalidHeapAddressError :: Val -> a
invalidHeapAddressError addr =
  error $ "lookupClosure: Invalid address " ++ pretty addr

updateClosure :: Heap -> Val -> Closure -> Heap
updateClosure heap addr new_cl = case addr of
  SLoc x -> heap{ heapStaticArea = M.alter f x (heapStaticArea heap) }
  Loc l -> heap{ heapAllocArea = IM.alter f l (heapAllocArea heap) }
 where
   f (Just _) = Just new_cl
   f Nothing = invalidHeapAddressError addr

allocClosure :: Heap -> Closure -> (Heap, Val)
allocClosure heap@Heap{ heapPtr = ptr } new_cl =
  (heap{ heapAllocArea = IM.insert ptr new_cl (heapAllocArea heap)
       , heapPtr = ptr + 1 }, Loc ptr)

alloc_static_closure :: Heap -> Id -> Closure -> Heap
alloc_static_closure heap x cl =
  heap{ heapStaticArea = M.insert x cl (heapStaticArea heap) }

loadBCOs :: FinalBCOs -> (InfoTables, Heap)
loadBCOs bcos =
  foldl' load1 (M.empty, Heap M.empty IM.empty 0) (M.toList bcos)
 where
   load1 (itbls, heap) (x, BcConInfo{ bcoConTag = tag }) =
     (M.insert x (ConstrInfoTable tag) itbls, heap)
   load1 (itbls, heap) (x, bco@BcoCon{}) =
     (itbls, alloc_static_closure heap x $
              Closure (bcoDataCon bco)
                      (V.fromList (map (either constToVal SLoc)
                                       (bcoFields bco))))
   load1 (itbls, heap) (x, bco@BcObject{}) =
     (M.insert x (CodeInfoTable (closure_type (bcoType bco))
                                (bcoCode bco)) itbls,
      alloc_static_closure heap x (Closure x V.empty))

   closure_type (BcoFun arity) = CtFun arity
   closure_type Thunk = CtThunk
   closure_type CAF = CtCAF

-- -------------------------------------------------------------------

ppArgStack :: ArgStack -> Heap -> IO PDoc
ppArgStack (ArgStack base top vals) heap = do
  elems <- getElems vals
  let (saved, frame0) = splitAt base elems
      frame = take (top - base) frame0
  return $
    text "Stack" <+> ppr base <> char '-' <> ppr top <+>
      align (--fillSep (commaSep (map ppr saved)) <+> char '|' <+>
             fillSep (commaSep (map ppr frame)) $+$
             ppHeapFromRoots (last saved : frame) heap)

-- | Pretty-print transitive closure of heap values.
--
-- Follows all pointers starting from the roots, prints the objects
-- they point to, and then does the same for all pointers contained
-- in the printed objects.
--
-- Each object is printed only once.
ppHeapFromRoots :: [Val]  -- ^ Roots.  Non-pointer values are ignored.
                -> Heap -> PDoc
ppHeapFromRoots roots heap =
  fillSep $ commaSep $ go IS.empty [ l | Loc l <- roots ]
 where
   go seen [] = []
   go seen (l:ls)
     | l `IS.member` seen = go seen ls
   go seen (l:ls) =
     let obj@(Closure _ fields) = lookupClosure heap (Loc l) in
     (ppr (Loc l) <> char '=' <> ppr obj) :
        go (IS.insert l seen)
           ([ l' | Loc l' <- V.toList fields ] ++ ls)

-- TODO: should be stack-frame number of nested stack frames, not words.
stackDepth :: ArgStack -> Int
stackDepth (ArgStack base _ _) = base

mkArgStack :: IO ArgStack
mkArgStack = do
  argStack <- newArray (0, 4096) Undef
  return $ ArgStack 0 0 argStack

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
    Lst (Call (Just (BcReg dst, _)) _ _) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    Lst (Eval _ (BcReg dst)) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    other ->
      error $ "Not returning to Call or Eval: " ++ pretty other

writeReg :: ArgStack -> Int -> Val -> IO ()
writeReg (ArgStack base top args) reg val
  | let reg_idx = base + reg, reg_idx < top = do
      writeArray args reg_idx val
writeReg _ reg _ =
  error $ "Register " ++ show reg ++ " not accessible in current frame."

-- | Read a register.  Register number must be in the range
-- @[(-1) .. framesize - 1]@.
--
-- Register @(-1)@ is the special @Node@ pointer which points to the
-- current closure and is used to access free variables.
readReg :: ArgStack -> Int -> IO Val
readReg (ArgStack base top args) reg
  | let reg_idx = base + reg, reg_idx < top, reg >= (-1) = do
      readArray args reg_idx
readReg _ reg =
  error $ "Register " ++ show reg ++ " not accessible in current frame."

instance Pretty Val where
  ppr (Loc p) = char '#' <> ppr p
  ppr (SLoc p) = char '#' <> ppr p
  ppr (ValI n) = ppr n
  ppr (ValS s) = text (show s)
  ppr Undef = text "undef"


instance Pretty Closure where
  ppr (Closure info fields) =
    parens $ sep (ppr info : map ppr (V.toList fields))

instance Pretty Heap where
  ppr Heap{ heapStaticArea = s, heapAllocArea = m } =
    text "Static Closures:" $+$ indent 2 (ppr s) $+$
    text "Heap:" $+$ indent 2 (ppr m)

getField :: Closure -> Int -> Val
getField (Closure _ fields) field_id
  | field_id > 0, field_id <= V.length fields
  = fields V.! (field_id - 1)
getField obj field_id = invalidFieldIdError obj field_id

setField :: Closure -> Int -> Val -> Closure
setField (Closure dcon fields) field_id val
  | field_id > 0, field_id <= V.length fields
  = Closure dcon (fields V.// [(field_id, val)])
setField obj field_id _ = invalidFieldIdError obj field_id

invalidFieldIdError :: Closure -> Int -> a
invalidFieldIdError obj field_id =
  error $ pretty $
    text "Invalid field id:" <+> ppr field_id <+>
    text "for object" <+> ppr obj

constToVal :: BcConst -> Val
constToVal (CInt n) = ValI n
constToVal (CStr s) = ValS s

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
interpSteps :: Int
            -> InfoTables -> Heap -> InfoTable -> Int -> ArgStack
            -> IO (Heap, ArgStack)
interpSteps n env hp _ _ args | n < 0 = do
  putStrLn $ "DONE. Steps remaining: " ++ show (-n)
  return (hp, args)
interpSteps 0 env hp _ _ args = do
  putStrLn "STOPPING (ran out of steps)"
  return (hp, args)
interpSteps n env hp bco pc args =
  interp1 env hp bco pc args (interpSteps (n-1)) (interpSteps (-n))

-- | Interpret current instruction and call continuation with modified
-- interpreter state.
interp1 :: InfoTables -> Heap -> InfoTable -> Int -> ArgStack
        -> (InfoTables -> Heap -> InfoTable -> Int -> ArgStack -> IO a)
        -> (InfoTables -> Heap -> InfoTable -> Int -> ArgStack -> IO a)
        -> IO a
interp1 env heap bco@CodeInfoTable{ itblCode = code } pc args k kstop = do
  let inst = fc_code code V.! pc
  stack <- ppArgStack args heap
  pprint $ text ">>>" <+> indent (stackDepth args)
                            (ppr pc <> colon <+> ppr inst $+$
                             text "..." <+> stack)
  case inst of
    Mid (Assign (BcReg dest) rhs) -> interp_rhs dest rhs
    Mid (Store base offs src) -> store base offs src
    Lst (Call (Just (dst, _)) fun vars) -> call dst fun vars
    Lst (Call Nothing fun vars) -> tailcall fun vars
    Lst (Ret1 (BcReg x)) -> do
      rslt <- readReg args x
      (args', bco', pc') <- popFrameRet env heap args rslt
      k env heap bco' pc' args'
    Lst (Goto pc') -> k env heap bco pc' args
    Lst (Case CaseOnTag (BcReg x) alts) -> case_branch x alts
    Lst (Eval pc' (BcReg reg)) -> assert (pc' == pc + 1) $ eval reg
    Lst Update -> update
    Lst Stop -> kstop env heap bco pc args
 where
   interp_rhs dst (Move (BcReg src)) = do
     pprint =<< ppArgStack args heap
     old_src <- readReg args src
     writeReg args dst old_src
     k env heap bco (pc + 1) args
   interp_rhs dst (BinOp op Int32Ty (BcReg r1) (BcReg r2)) = do
     ValI v1 <- readReg args r1
     ValI v2 <- readReg args r2
     writeReg args dst (bin_op_int32ty op v1 v2)
     k env heap bco (pc + 1) args
   interp_rhs dst (Load (LoadLit c)) = do
     writeReg args dst (constToVal c)
     k env heap bco (pc + 1) args
   interp_rhs dst (Load (LoadGlobal x)) = do
     writeReg args dst (SLoc x)
     k env heap bco (pc + 1) args
   interp_rhs dst (Load LoadBlackhole) = do
     writeReg args dst (SLoc blackholeDataConId)
     k env heap bco (pc + 1) args
   interp_rhs dst (Load (LoadClosureVar i)) = do
     node_ptr <- readReg args (-1)
     let Closure _ free_vars = lookupClosure heap node_ptr
     writeReg args dst (free_vars V.! (i - 1))
     k env heap bco (pc + 1) args
   interp_rhs dst (Alloc (BcReg con_info) vars) = do
     SLoc dcon <- readReg args con_info
     alloc dst dcon =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs dst (AllocAp vars) = do
     let apsize = ValI (fromIntegral (length vars))
     alloc dst apDataConId . (apsize:) =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs dst (Fetch (BcReg src) offs) = do
     ptr <- readReg args src
     let cl = lookupClosure heap ptr
     writeReg args dst (getField cl offs)
     k env heap bco (pc + 1) args

   alloc dst dcon vals = do
     let cl = Closure dcon (V.fromList vals)
     let (heap', ptr) = allocClosure heap cl
     writeReg args dst ptr
     k env heap' bco (pc + 1) args

   store (BcReg dst) offs (BcReg src) = do
     ptr <- readReg args dst
     val <- readReg args src
     -- write barrier goes somewhere below
     let cl = lookupClosure heap ptr
         cl' = setField cl offs val
         heap' = updateClosure heap ptr cl'
     k env heap' bco (pc + 1) args

   call (BcReg _dst) (BcReg fun) vars = do
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
            k env heap itbl' 0 args'

   tailcall (BcReg fun) vars = do
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
            k env heap itbl' 0 args'

   case_branch reg alts = do
     addr <- readReg args reg
     let Closure itbl _ = lookupClosure heap addr
     let bco' = lookupInfoTable env itbl
     case bco' of
       ConstrInfoTable{ itblTag = tag } -> do
         let pc' = find_alt tag alts
         --pprint $ text "Found tag:" <+> ppr tag <+> text "->" <+> ppr pc'
         k env heap bco pc' args
    where
      find_alt tag ((DefaultTag, offs) : alts') = find_alt' tag alts' offs
      find_alt tag alts' = find_alt' tag alts' (error "Unmatched pattern")
      find_alt' tag [] dflt = dflt
      find_alt' tag ((Tag tag', dst) : alts') dflt
        | tag == tag' = dst
        | otherwise   = find_alt' tag alts' dflt

   eval reg = do
     node_ptr <- readReg args reg
     let Closure itbl_id _ = lookupClosure heap node_ptr
     case lookupInfoTable env itbl_id of
       ConstrInfoTable _ -> k env heap bco (pc + 1) args
       bco'@CodeInfoTable{ itblClosureType = cl_ty }
         | CtFun _ <- cl_ty -> k env heap bco (pc + 1) args
         | otherwise -> eval_it bco' node_ptr

   eval_it bco'@CodeInfoTable{ itblCode = code } node_ptr = do
     (args', pc') <- pushUpdateFrame args (pc+1) node_ptr
     -- like call
     args'' <- allocFrame args' pc' node_ptr [] (fc_framesize code)
     pprint $ text (replicate (stackDepth args'' + 4) '*') <+> (text "Eval-Entering:" <+> ppr node_ptr)
     k env heap bco' 0 args''

   update = do
     -- Semantics: *R(0) = *R(1)  // or create indirection
     --            return R(1)  -or-  R(0) = R(1); return R(0)
     old_ptr <- readReg args 0
     new_ptr <- readReg args 1
     let new_cl = lookupClosure heap new_ptr
         heap' = updateClosure heap old_ptr new_cl
     (args', bco', pc') <- popFrameRet env heap' args new_ptr
     k env heap' bco' pc' args'


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
--  | R0        | <- Used to receive the result from evaluation.
--  +-----------+
--  | R1        | <- Used to receive the result
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
     [Lst (Call (Just (BcReg 1, 1)) (BcReg 1) [])
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
     [Lst (Eval 1 (BcReg 0))
     ,Lst Stop]

test_insts2 :: FinalBCOs -> IO ()
test_insts2 bcos0 = do
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