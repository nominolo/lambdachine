{-# LANGUAGE BangPatterns, PatternGuards, GADTs #-}
module Lambdachine.Grin.Interp where

import Lambdachine.Grin.Bytecode
import Lambdachine.Id
import Lambdachine.Grin.RegAlloc ( LinearCode(..) )
import Lambdachine.Utils
import Lambdachine.Builtin

import Control.Monad ( forM_ )
import Data.Array.IO
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

data Obj = Obj !Id (V.Vector Val)

type BCO = BytecodeObject' FinalCode

data Heap = Heap {-# UNPACK #-} !Int (IM.IntMap Obj)
type StaticEnv = M.Map Id (Either Obj BCO)

data PC = PC BCO {-# UNPACK #-} !Int
type CallStack = [PC]
data ArgStack = ArgStack
  { argBase :: {-# UNPACK #-} !Int
  , argTop  :: {-# UNPACK #-} !Int
  , argVals :: IOArray Int Val
  }

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
     let obj@(Obj _ fields) = lookupHeap heap l in
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

popFrameRet :: StaticEnv -> Heap -> ArgStack -> Val
            -> IO (ArgStack, BCO, Int)
popFrameRet env heap (ArgStack base top args) result = do
  let !prev_top = base - 3
  ValI prev_base <- readArray args prev_top
  ValI old_pc <- readArray args (prev_top + 1)
  let base' = fromIntegral prev_base
  let pc' = fromIntegral old_pc
  old_node <- readArray args (base' - 1)
  let bco = getBCO env heap old_node
--   pprint $ text "Trying to return to:" <+>
--              align (ppr pc' <+> ppr old_node <+> ppr bco)
  case fc_code (bcoCode bco) V.! (pc' - 1) of
    Lst (Call (Just (BcReg dst, _)) _ _) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    Lst (Eval _ (BcReg dst)) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')
    other ->
      error $ "Not returning to Call or Eval: " ++ pretty other

getBCO :: StaticEnv -> Heap -> Val -> BCO
getBCO env hp (SLoc x) =
  let Right bco = lookupStaticEnv env x in bco
getBCO env hp (Loc l) =
  let Obj dcon _ = lookupHeap hp l
      Right bco = lookupStaticEnv env dcon
  in bco

getObj :: StaticEnv -> Heap -> Val -> Obj
getObj env _heap (SLoc x) =
  let Left obj = lookupStaticEnv env x in obj
getObj _env heap (Loc l) = lookupHeap heap l

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

instance Pretty Obj where
  ppr (Obj con fields) =
    parens $ sep (ppr con : map ppr (V.toList fields))

instance Pretty Heap where
  ppr (Heap _ m) = ppr m

interp :: Id -> FinalBCOs -> (Heap, StaticEnv)
interp entry bcos = (heap0, static0)
 where
   (heap0, static0) = initHeap bcos

initHeap :: FinalBCOs -> (Heap, StaticEnv)
initHeap bcos = (Heap 0 IM.empty, M.map convert_bco bcos)
 where
   convert_bco BcoCon{ bcoDataCon = dcon, bcoFields = fields } =
     Left (mkObj dcon fields)
   convert_bco bco@BcObject{} = Right bco
   convert_bco bco@BcConInfo{} = Right bco

heapAlloc :: Heap -> Obj -> (Val, Heap)
heapAlloc (Heap next hp) obj =
  (Loc next, Heap (next + 1) (IM.insert next obj hp))

lookupHeap :: Heap -> Int -> Obj
lookupHeap (Heap _ hp) loc = fromMaybe err $ IM.lookup loc hp
 where
   err = invalidHeapLocationError loc

invalidHeapLocationError :: Int -> a
invalidHeapLocationError loc =
  error $ "Not a valid Heap location: " ++ show loc

updateHeap :: Heap -> Int -> Obj -> Heap
updateHeap (Heap next hp) loc obj' =
  Heap next $ IM.alter f loc hp
 where
   f (Just _) = Just obj'
   f Nothing = invalidHeapLocationError loc

lookupStaticEnv :: StaticEnv -> Id -> Either Obj BCO
lookupStaticEnv env x = fromMaybe err $ M.lookup x env
 where
   err = error $ pretty $ text "lookupStaticEnv: Unknown id:" <+> ppr x

updateStaticEnv :: StaticEnv -> Id -> Either Obj BCO -> StaticEnv
updateStaticEnv env x thing = M.insert x thing env

iEval :: Val -> Heap -> StaticEnv -> (Val, Heap, StaticEnv)
iEval val@(SLoc x) hp env =
  case env M.! x of
    Left obj -> (val, hp, env)
    Right bco -> undefined

inWhnf :: StaticEnv -> Obj -> Bool
inWhnf env (Obj con _) =
  case env M.! con of
    Right bco ->
      case bcoType bco of
        Con -> True
        BcoFun _ -> True
        _ -> False

mkObj :: Id -> [Either BcConst Id] -> Obj
mkObj dcon fields = Obj dcon (V.fromList (map toVal fields))
 where
   toVal (Right x) = SLoc x
   toVal (Left c) = constToVal c

getField :: Obj -> Int -> Val
getField (Obj _ fields) field_id
  | field_id > 0, field_id <= V.length fields
  = fields V.! (field_id - 1)
getField obj field_id = invalidFieldIdError obj field_id

setField :: Obj -> Int -> Val -> Obj
setField (Obj dcon fields) field_id val
  | field_id > 0, field_id <= V.length fields
  = Obj dcon (fields V.// [(field_id, val)])
setField obj field_id _ = invalidFieldIdError obj field_id

invalidFieldIdError :: Obj -> Int -> a
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

pre_post :: [Val] -> [FinalIns] -> [Maybe Val] -> IO Bool
pre_post pre code post = do
  let frame_size = (length pre)
  args <- addFrame frame_size =<< mkArgStack
  forM_ (zip [0..] pre) $ \(n, val) -> writeReg args n val
  let (heap, env) = initHeap M.empty
  let bco = mkDummyFun frame_size code
  (_, heap', args') <- interpSteps (V.length (fc_code (bcoCode bco))) env heap bco 0 args []
  arg_elems <- take frame_size `fmap` getElems (argVals args')
  pprint (arg_elems, heap')
  return $ and $ zipWith verify_arg post arg_elems
 where
   verify_arg Nothing _ = True
   verify_arg (Just v) v' = v == v'

interpSteps :: Int
            -> StaticEnv -> Heap -> BCO -> Int -> ArgStack -> [PC]
            -> IO (StaticEnv, Heap, ArgStack)
interpSteps 0 env hp _ _ args _ = do
  putStrLn "STOPPING (ran out of steps)"
  return (env, hp, args)
interpSteps n env hp bco pc args callstack =
  interp1 env hp bco pc args callstack (interpSteps (n-1))

-- | Interpret current instruction and call continuation with modified
-- interpreter state.
interp1 :: StaticEnv -> Heap -> BCO -> Int -> ArgStack -> [PC]
        -> (StaticEnv -> Heap -> BCO -> Int -> ArgStack -> [PC] -> IO a)
        -> IO a
interp1 env heap bco@BcObject{ bcoCode = code } pc args callstack k = do
  let inst = fc_code code V.! pc
  stack <- ppArgStack args heap
  pprint $ text ">>>" <+> indent (stackDepth args)
                            (ppr pc <> colon <+> ppr inst $+$
                             text "..." <+> stack)
  case inst of
    Mid (Assign (BcReg dest) rhs) -> interp_rhs dest rhs
    Mid (Store base offs src) -> write base offs src
    Lst (Call (Just (dst, _)) fun vars) -> call dst fun vars
    Lst (Call Nothing fun vars) -> tailcall fun vars
    Lst (Ret1 (BcReg x)) -> do
      rslt <- readReg args x
      (args', bco', pc') <- popFrameRet env heap args rslt
      k env heap bco' pc' args' callstack
    Lst (Goto pc') -> k env heap bco pc' args callstack
    Lst (Case CaseOnTag (BcReg x) alts) -> case_branch x alts
    Lst (Eval pc' (BcReg reg)) -> assert (pc' == pc + 1) $ eval reg
    Lst Update -> update
    Lst Stop -> return undefined
 where
   interp_rhs dst (Move (BcReg src)) = do
     pprint =<< ppArgStack args heap
     old_src <- readReg args src
     writeReg args dst old_src
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (BinOp op Int32Ty (BcReg r1) (BcReg r2)) = do
     ValI v1 <- readReg args r1
     ValI v2 <- readReg args r2
     writeReg args dst (bin_op_int32ty op v1 v2)
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (Load (LoadLit c)) = do
     writeReg args dst (constToVal c)
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (Load (LoadGlobal x)) = do
     writeReg args dst (SLoc x)
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (Load LoadBlackhole) = do
     writeReg args dst (SLoc blackholeDataConId)
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (Load (LoadClosureVar i)) = do
     node <- readReg args (-1)
     let Obj _ free_vars = getObj env heap node
     writeReg args dst (free_vars V.! (i - 1))
     k env heap bco (pc + 1) args callstack
   interp_rhs dst (Alloc (BcReg con_info) vars) = do
     SLoc dcon <- readReg args con_info
     alloc dst dcon =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs dst (AllocAp vars) = do
     let apsize = ValI (fromIntegral (length vars))
     alloc dst apDataConId . (apsize:) =<< mapM (\(BcReg r) -> readReg args r) vars
   interp_rhs dst (Fetch (BcReg src) offs) = do
     ptr <- readReg args src
     case ptr of
       Loc p -> do
         let o = lookupHeap heap p
         writeReg args dst (getField o offs)
       SLoc x -> do
         let Left o = lookupStaticEnv env x
         writeReg args dst (getField o offs)
     k env heap bco (pc + 1) args callstack

   alloc dst dcon vals = do
     let obj = Obj dcon (V.fromList vals)
     let (ptr, heap') = heapAlloc heap obj
     writeReg args dst ptr
     k env heap' bco (pc + 1) args callstack

   write (BcReg dst) offs (BcReg src) = do
     ptr <- readReg args dst
     val <- readReg args src
     -- write barrier goes somewhere below
     case ptr of
       Loc p ->
         let o = lookupHeap heap p
             heap' = updateHeap heap p (setField o offs val)
         in k env heap' bco (pc + 1) args callstack
       SLoc x ->
         error "Does a store into a static location make sense?"

   call (BcReg _dst) (BcReg fun) vars = do
     SLoc f <- readReg args fun
     let Right bco' = lookupStaticEnv env f
     let num_args = length vars
     case bcoType bco' of
       BcoFun arity
         | arity == num_args -> do
            vals <- mapM (\(BcReg r) -> readReg args r) vars
            args' <- allocFrame args (pc + 1) (SLoc f) vals (fc_framesize (bcoCode bco'))
            pprint $ text "***" <+> indent (stackDepth args') (text "Entering:" <+> ppr fun)
            k env heap bco' 0 args' callstack

   tailcall (BcReg fun) vars = do
     node_ptr@(SLoc f) <- readReg args fun
     let Right bco' = lookupStaticEnv env f
     let num_args = length vars
     case bcoType bco' of
       BcoFun arity
        | arity == num_args -> do
            -- copy arguments down
            vals <- mapM (\(BcReg r) -> readReg args r) vars
            forM_ (zip [0..] vals) $ \(i, v) -> writeReg args i v
            writeReg args (-1) node_ptr
            args' <- adjustFramesize args (fc_framesize (bcoCode bco'))
            pprint $ text (replicate (stackDepth args' + 4) '*') <+> (text "Entering:" <+> ppr f)
            k env heap bco' 0 args' callstack

   case_branch reg alts = do
     addr <- readReg args reg
     let Obj ctor _ = getObj env heap addr
     let Right bco' = lookupStaticEnv env ctor
     case bco' of
       BcConInfo{ bcoConTag = tag } -> do
         let pc' = find_alt tag alts
         --pprint $ text "Found tag:" <+> ppr tag <+> text "->" <+> ppr pc'
         k env heap bco pc' args callstack
    where
      find_alt tag ((DefaultTag, offs) : alts') = find_alt' tag alts' offs
      find_alt tag alts' = find_alt' tag alts' (error "Unmatched pattern")
      find_alt' tag [] dflt = dflt
      find_alt' tag ((Tag tag', dst) : alts') dflt
        | tag == tag' = dst
        | otherwise   = find_alt' tag alts' dflt

   eval reg = do
     node_ptr <- readReg args reg
     case node_ptr of
       SLoc x ->
         case lookupStaticEnv env x of
           Left obj -> k env heap bco (pc + 1) args callstack
           Right bco' ->
             case bco' of
               BcObject{ bcoType = obj_ty }
                 | CAF <- obj_ty -> eval_it bco' node_ptr
                 | BcoFun _ <- obj_ty ->
                     k env heap bco (pc + 1) args callstack
       Loc l ->
         let Obj ctor _ = lookupHeap heap l
             Right bco' = lookupStaticEnv env ctor
         in case bco' of
              BcConInfo{} -> k env heap bco (pc + 1) args callstack
              BcObject{ bcoType = obj_ty }
                | Thunk <- obj_ty -> eval_it bco' node_ptr
                | BcoFun _ <- obj_ty ->
                    k env heap bco (pc + 1) args callstack

   eval_it bco'@BcObject{ bcoCode = code } node_ptr = do
     (args', pc') <- pushUpdateFrame args (pc+1) node_ptr
     -- like call
     args'' <- allocFrame args' pc' node_ptr [] (fc_framesize code)
     pprint $ text (replicate (stackDepth args'' + 4) '*') <+> (text "Eval-Entering:" <+> ppr node_ptr)
     k env heap bco' 0 args'' callstack

   update = do
     -- Semantics: *R(0) = *R(1)  // or create indirection
     --            return R(1)  -or-  R(0) = R(1); return R(0)
     old_val <- readReg args 0
     new_val <- readReg args 1
     let new_obj = case new_val of
                     SLoc x -> lookupStaticEnv env x
                     Loc l -> Left $ lookupHeap heap l
     let (env', heap') =
           case old_val of
             SLoc y ->
               (updateStaticEnv env y new_obj, heap)
             Loc l ->
               case new_obj of
                 Left o' -> (env, updateHeap heap l o')
                 Right _ -> error "Cannot currently update a dynamic object with a static object."

     (args', bco', pc') <- popFrameRet env' heap' args new_val
     k env' heap' bco' pc' args' callstack


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
      bco = initCodeBCO
      pc = 0
      (heap, env) = initHeap bcos
      steps = 1000
  (_, heap', args') <- interpSteps steps env heap bco pc args []
  --arg_elems <- take frame_sizea `fmap` getElems (argVals args')
  pprint =<< ppArgStack args' heap'
  pprint heap'

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
