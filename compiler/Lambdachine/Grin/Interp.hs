{-# LANGUAGE BangPatterns, PatternGuards, GADTs #-}
module Lambdachine.Grin.Interp where

import Lambdachine.Grin.Bytecode
import Lambdachine.Id
import Lambdachine.Grin.RegAlloc ( LinearCode(..) )
import Lambdachine.Utils.Pretty
import Lambdachine.Builtin

import Control.Monad ( forM_ )
import Data.Array.IO
import Data.Maybe ( fromMaybe )
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Map    as M

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

ppArgStack :: ArgStack -> IO PDoc
ppArgStack (ArgStack base top vals) = do
  elems <- getElems vals
  return $ text "Stack" <+> ppr base <> char '-' <> ppr top <+>
             sep (commaSep (map ppr (take (top - base) elems)))

mkArgStack :: IO ArgStack
mkArgStack = do
  argStack <- newArray_ (0, 4096)
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
  let !top' = base + stackFrameSize + framesize
  if top' >= max_top then
    error "Stack Overflow"
   else do
    writeArray args top (ValI (fromIntegral base))  -- prevFrame
    writeArray args (top + 1) (ValI (fromIntegral pc))  -- savedPC
    writeArray args (top + 2) fun  -- curNode
    forM_ (zip [top + 3 ..] params) $ \(i, v) ->
      writeArray args i v
    return $ ArgStack (top + 3) top' args

adjustFramesize :: ArgStack -> Int -> IO ArgStack
adjustFramesize (ArgStack base top args) new_framesize =
  let !top' = base + new_framesize in
  if top' <= top then
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
  bco <- getBCO env heap `fmap` readArray args (prev_top + 2)
  case fc_code (bcoCode bco) V.! (pc' - 1) of
    Lst (Call (Just (BcReg dst, _)) _ _) -> do
      writeArray args (base' + dst) result
      return (ArgStack base' prev_top args, bco, pc')

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

{-
interp1 :: StaticEnv -> Heap -> BCO -> Int -> Stack
        -> (StaticEnv, Heap, BCO, Int)
interp1 env hp pc_bco pc_offs =
  case pc_bco V.! pc_offs of
    Mid (Assign (Reg dst)
-}

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
interpSteps 0 env hp _ _ args _ = return (env, hp, args)
interpSteps n env hp bco pc args callstack =
  interp1 env hp bco pc args callstack (interpSteps (n-1))

-- | Interpret current instruction and call continuation with modified
-- interpreter state.
interp1 :: StaticEnv -> Heap -> BCO -> Int -> ArgStack -> [PC]
        -> (StaticEnv -> Heap -> BCO -> Int -> ArgStack -> [PC] -> IO a)
        -> IO a
interp1 env heap bco@BcObject{ bcoCode = code } pc args callstack k = do
  let inst = fc_code code V.! pc
  pprint $ text ">>>" <+> ppr inst
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
 where
   interp_rhs dst (Move (BcReg src)) = do
     pprint =<< ppArgStack args
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
     writeReg args dst (free_vars V.! i)
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
            k env heap bco' 0 args' callstack

   tailcall (BcReg fun) vars = do
     SLoc f <- readReg args fun
     let Right bco' = lookupStaticEnv env f
     let num_args = length vars
     case bcoType bco' of
       BcoFun arity
        | arity == num_args -> do
            -- copy arguments down
            vals <- mapM (\(BcReg r) -> readReg args r) vars
            forM_ (zip [0..] vals) $ \(i, v) -> writeReg args i v
            args' <- adjustFramesize args (fc_framesize (bcoCode bco))
            k env heap bco' 0 args' callstack

   case_branch reg alts = do
     undefined

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
