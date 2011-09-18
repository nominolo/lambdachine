{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Lambdachine.Serialise where

import Lambdachine.Id
import Lambdachine.Grin.Bytecode
import Lambdachine.Utils
import Lambdachine.Utils.IO
import Lambdachine.Utils.Pretty
import Lambdachine.Utils.Convert

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.Vector as V
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.Char8 ( fromString )
import Control.Applicative
import Control.Exception ( assert )
import Control.Monad ( liftM, ap, forM_, forM )
import Control.Monad.State.Strict --( StateT, runStateT )
import Data.Bits
import Data.Char ( ord )
import Data.List ( sortBy, find )
import Data.Monoid
import Data.Ord ( comparing )
import Data.Word
import Debug.Trace ( trace )
import System.IO ( Handle )
import qualified Data.Serialize.References as R

#include "../Opcodes.h"

{-
data BuildState = BuildState
  { bsStringTable :: M.Map B.ByteString Word
  , bsNextStringId :: {-# UNPACK #-} Word
  }
-}
newtype Build a =
  Build { unBuild :: forall r.
                     M.Map B.ByteString Word
                  -> Word
                  -> (M.Map B.ByteString Word
                        -> Word
                        -> a
                        -> R.BuildM r)
                  -> R.BuildM r }

runBuild :: Build a -> R.BuildM (M.Map B.ByteString Word, a)
runBuild b = unBuild b M.empty 0 (\str _ a -> return (str, a))

getStringTable' :: Build (M.Map B.ByteString Word)
getStringTable' = Build (\st n k -> k st n st)

instance Monad Build where
  return a = Build (\st sti k -> k st sti a)
  Build f >>= kont = Build (\st sti k ->
    f st sti (\st' sti' a ->
      unBuild (kont a) st' sti' k))

instance Applicative Build where
  pure = return
  (<*>) = ap

instance Functor Build where
  fmap = liftM

newtype BuildM a =
  BuildM { unBuildM :: forall r.
                       M.Map B.ByteString Word
                    -> Word
                    -> (Builder -> Builder)
                    -> (M.Map B.ByteString Word
                          -> Word
                          -> (Builder -> Builder)
                          -> a
                          -> r)
                    -> r }

instance Monad BuildM where
  return x = BuildM $ \s n b k -> k s n b x
  BuildM f >>= kont = BuildM $ \s n b k ->
    f s n b (\s' n' b' a -> unBuildM (kont a) s' n' b' k)

instance Functor BuildM where fmap = liftM
instance Applicative BuildM where pure = return; (<*>) = ap

-- class Serialise a where
--   put :: a -> BuildM ()

addString :: B.ByteString -> BuildM Word
addString s = BuildM $ \tbl n b k ->
  case M.lookup s tbl of
    Just m -> k tbl n b m
    Nothing -> let !m = n + 1 in k (M.insert s n tbl) m b n

getStringTable :: BuildM (M.Map B.ByteString Word)
getStringTable = BuildM $ \tbl n b k -> k tbl n b tbl

emit :: Builder -> BuildM ()
emit b' = BuildM $ \tbl n bf k -> k tbl n (\b -> bf b `mappend` b') ()

runBuildM :: BuildM a -> (a, M.Map B.ByteString Word, Builder)
runBuildM m = unBuildM m M.empty 0 id (\tbl _ bf a -> (a, tbl, bf mempty))

captureOutput :: BuildM a -> BuildM (a, Builder)
captureOutput (BuildM f) = BuildM $ \tbl n bf0 k ->
  f tbl n id $ \tbl' n' bf a ->
  k tbl' n' bf0 (a, bf mempty)

{-
test1 = (tbl, L.unpack $ toLazyByteString b, L.unpack $ toLazyByteString o)
 where
   (o, tbl, b) = runBuildM $ do
      s <- addString (B.pack [65,66,67])
      putIns (fromIntegral s)
      t <- addString (B.pack [68,69,70])
      (_, o0) <- captureOutput $ putIns (fromIntegral t)
      putIns 42
      return o0
-}
writeModule :: FilePath -> BytecodeModule -> IO ()
writeModule path bcos = L.writeFile path (encodeModule bcos)

hWriteModule :: Handle -> BytecodeModule -> IO ()
hWriteModule hdl bcos = L.hPut hdl (encodeModule bcos)

encodeModule :: BytecodeModule -> L.ByteString
encodeModule mdl = encodeModule' mdl
{-
   let out = toLazyByteString builder
       out' = encodeModule' mdl
   in assertEqualLBS out out' out
 where
   bcos = bcm_bcos mdl
   imports = bcm_imports mdl

   builder = mconcat
     [ fromString "KHCB" -- magic
     , fromWrite (writeWord16be 0 `mappend` writeWord16be 1) -- version
     , fromWrite (writeWord32be 0) -- flags
     , fromWrite (writeWord32be (fromIntegral (M.size strings)))
     , fromWrite (writeWord32be (fromIntegral numItbls))
     , fromWrite (writeWord32be (fromIntegral numClosures))
     , fromWrite (writeWord32be (fromIntegral (length imports)))
     , fromString "BCST" -- string table section magic
     , encodeStringTable strings
     , mdl_outp
     , fromString "BCCL" -- closure table section magic
     , output
     ]

   ((mdl_outp, numItbls, numClosures), strings, output) =
     runBuildM buildit
   buildit = do
     (_, name_and_imports)
       <- captureOutput $ do
               encodeIdString (bcm_name mdl)
               mapM_ encodeIdString imports
     itbls <- sum <$> (forM (M.toList bcos) $ \(name, bco) ->
                         encodeInfoTable name bco)
     closures <- sum <$> (forM (M.toList bcos) $ \(name, bco) ->
                            encodeClosure name bco)
     return (name_and_imports, itbls, closures)
   encodeInfoTable :: Id -> BytecodeObject' FinalCode
                -> BuildM Word
   encodeInfoTable name bco =
     case bco of
       BcConInfo tag fields tys -> do
         emit $ fromString "ITBL"
         encodeId name
         emit $ varUInt cltype_CONSTR
         emit $ varUInt (fromIntegral tag)
         assert (fields == length tys) $ do
         encodePointerMask tys
         encodeId name
         return 1
       BcObject{ bcoType = ty }
         | BcoFun arity <- ty -> do
            let itblName = mkInfoTableId (idName name)
            emit $ fromString "ITBL"
            encodeId itblName
            emit $ varUInt cltype_FUN
            encodePointerMask (map snd (M.toList (bcoFreeVars bco)))
            encodeId itblName
            encodeCode arity (bcoCode bco)
            return 1
         | ty `elem` [Thunk, CAF] -> do
            let itblName = mkInfoTableId (idName name)
            emit $ fromString "ITBL"
            encodeId itblName
            emit $ varUInt cltype_THUNK
            encodePointerMask (map snd (M.toList (bcoFreeVars bco)))
            encodeId itblName
            encodeCode 0 (bcoCode bco)
            return 1

       BcoCon{ } ->
         return 0

       BcTyConInfo{ } -> return 0

   -- Create the closure part for a BCO
   encodeClosure name bco =
     case bco of
       BcConInfo _ _ _ -> return 0
       BcoCon _ con_id fields -> do
         emit $ fromString "CLOS"
         encodeId name
         emit $ varUInt (fromIntegral (length fields))
         encodeId con_id
         mapM_ encodeField fields
         return 1
       BcObject{ bcoType = BcoFun arity, bcoFreeVars = fvs }
         | M.size fvs == 0 -> do
           emit $ fromString "CLOS"
           encodeId name
           emit $ varUInt 0  -- no payload
           encodeId (mkInfoTableId (idName name)) -- info table
           -- no payload, hence no literals
           return 1
         | otherwise ->
           -- A function with free variables need not have a static
           -- closure.
           return 0
       BcObject{ bcoType = CAF, bcoFreeVars = fvs } | M.size fvs == 0 -> do
         emit $ fromString "CLOS"
         encodeId name
         emit $ varUInt 1  -- one word for the indirection
         encodeId (mkInfoTableId (idName name))  -- info table
         encodeField (Left (CInt 0))
         return 1
       BcObject{ bcoType = Thunk } ->
         return 0  -- don't need a static closure
       BcTyConInfo{ } ->
         return 0
       _ ->
         error $ "UNIMPL: encodeClosure: " ++ pretty bco

   encodeField :: Either BcConst Id -> BuildM ()
   encodeField lit = case lit of
     Left (CInt n) -> do
       emit $ varUInt littype_INT
       emit $ varSInt (fromIntegral n)
     Left (CStr s) -> do
       emit $ varUInt littype_STRING
       sid <- addString (U.fromString s)
       emit $ varUInt sid
     Left (CChar c) -> do
       emit $ varUInt littype_CHAR
       emit $ varUInt (fromIntegral (ord c))
     Left (CWord n) -> do
       emit $ varUInt littype_WORD
       emit $ varUInt (fromIntegral n)
     Left (CFloat r) -> do
       emit $ varUInt littype_FLOAT
       emit $ fromWrite $ writeWord32be $ floatToWord32 $ fromRational r
     Right x -> do
       case idDetails x of
         InfoTableId -> emit $ varUInt littype_INFO
         DataConInfoTableId -> emit $ varUInt littype_INFO
         _ -> emit $ varUInt littype_CLOSURE
       encodeId x

   encodeCode :: Int -> FinalCode -> BuildM ()
   encodeCode arity code = do
     emit $ varUInt (fromIntegral (fc_framesize code))
     emit $ varUInt (fromIntegral arity)
     let literals = collectLiterals code
         lit_ids = M.fromAscList (zip (S.toList literals) [0..])
     emit $ varUInt (fromIntegral (M.size lit_ids))
     let (codesize, _) = newAddresses code
     emit $ fromWord16be (i2h (codesize + 1))
     encodeLiterals lit_ids
     emit $ fromWrite $ writeWord32be $
       insAD opc_FUNC (i2b (fc_framesize code)) 0
     encodeInstructions lit_ids code
     return ()
     --putLinearIns lit_ids (Lst Stop)  -- bytecode dummy

   encodeLiterals lit_ids = do
     forM_ (M.keys lit_ids) encodeField
-}
encodeId :: Id -> BuildM ()
encodeId the_id =
  encodeIdString (show the_id ++ type_suffix the_id)
 where
    type_suffix anId = case idDetails anId of
      TopLevelId -> "`closure"
      InfoTableId -> "`info"
      DataConId -> "`con"
      DataConInfoTableId -> "`con_info"
      _ -> "`other"

encodeIdString :: String -> BuildM ()
encodeIdString str = encodeId' (B.split dot (U.fromString str))
 where
   dot = fromIntegral (ord '.') :: Word8

encodeId' :: [B.ByteString] -> BuildM ()
encodeId' parts = do
  let n = length parts
  emit $ varUInt (fromIntegral n)
  forM_ parts $ \p -> do
    idx <- addString p
    emit $ varUInt idx

-- | Assumptions:
-- > M.elems tbl == [0 .. M.size - 1]
encodeStringTable :: M.Map B.ByteString Word -> Builder
encodeStringTable tbl =
  mconcat [ encOne str | (_, str) <- inverted_tbl ]
 where
   encOne str =
     varUInt (fromIntegral (B.length str))
       `mappend` fromByteString str
   inverted_tbl =
     sortBy (comparing fst) [ (i, str) | (str, i) <- M.toList tbl ]

br_bias :: Int
br_bias = branch_BIAS

--putIns :: Word32 -> BuildM ()
--putIns w = emit $ fromWrite (writeWord32be w)

encodeInstructions :: LiteralIds
                   -> FinalCode -> BuildM Int
encodeInstructions lits code = do
  let (len, addrs) = newAddresses code
  let inss = zip [0..] (V.toList (fc_code code))

  ((), bldr)
    <- captureOutput $
         runInsBuildM $ do
           forM_ inss $ \(ins_id, ins) -> do
             putLinearIns lits addrs ins_id ins
  let bs = toLazyByteString bldr
  if L.length bs /= fromIntegral (len * 4) then
    error $ "Size mismatch. expected: " ++ show (len * 4) ++ " got: "
            ++ show (L.length bs) ++ "\n"
            ++ show (L.unpack bs) ++ "\n"
            ++ pretty code
   else do
     emit $ fromLazyByteString bs
     return len

-- | Encode a pointer bitmap for use by the garbage collector.
-- Prefixed by the total size of the object (length of the input
-- list).
encodePointerMask :: [OpTy] -> BuildM ()
encodePointerMask ops = do
  emit (varUInt (fromIntegral (length ops)))
  emit (fromWord32sbe (pointer_bitmap 0 0 ops))
 where
   pointer_bitmap :: Word32 -> Int -> [OpTy] -> [Word32]
   pointer_bitmap !acc !shift []
     | shift == 0 = []
     | otherwise  = [acc]
   pointer_bitmap !acc !shift ts0@(t:ts)
     | shift > 31 = acc : pointer_bitmap 0 0 ts0
     | otherwise =
       let acc' | isGCPointer t = acc .|. (1 `shiftL` shift)
                | otherwise     = acc
       in pointer_bitmap acc' (shift + 1) ts

test_encodePointerMask1 =
  let (_, _, b) = runBuildM (encodePointerMask [PtrTy, IntTy, FloatTy, PtrTy, FunTy [IntTy] IntTy])
  in L.unpack (toLazyByteString b) == [5, 0,0,0,25]

test_encodePointerMask2 =
  let (_, _, b) = runBuildM (encodePointerMask [])
  in L.unpack (toLazyByteString b) -- == [5, 0,0,0,25]

-- IMPORTANT: must match implementation of putLinearIns
insLength :: FinalIns -> Int
insLength ins =
  let l = insLength' ins in l
  --trace ("insLength " ++ pretty ins ++ " => " ++ show l) l
insLength' :: FinalIns -> Int
insLength' ins = case ins of
  Lst Stop -> 1
  Lst (Ret1 _) -> 1
  Lst (Eval _ _ _) -> 3
  Lst (Call Nothing _ (_:args)) -> 1
  Lst (Call (Just _) _ (_:args)) -> 3 + arg_len args
  Lst (CondBranch _ _ _ _ _ _) -> 2
  Lst (Goto _) -> 1
  Lst Update -> 1
  Lst (Case casetype _ alts) -> alt_len casetype alts
  Mid (Assign d (Move s)) | d == s -> 0
  Mid (Assign _ (Alloc _ [_] _)) -> 2
  Mid (Assign _ (Alloc _ args _)) -> 2 + arg_len args
  Mid (Assign _ (AllocAp (_:args) _)) -> 2 + arg_len args
  Mid _ -> 1
 where
   ceilDiv4 x = (x + 3) `div` 4

   arg_len :: [a] -> Int
   arg_len args = ceilDiv4 (length args)

   alt_len :: CaseType -> [(BcTag, a, label)] -> Int
   alt_len casetype alts =
     let (_,_,_,len) = viewCaseAlts casetype alts in len

type NewAddresses = IM.IntMap Int

newAddresses :: FinalCode -> (Int, NewAddresses)
newAddresses code =
  V.ifoldl' upd (0, IM.empty) (fc_code code)
 where
   upd (addr, m) idx ins =
     (addr + insLength ins, IM.insert idx addr m)

data UseCaseEncoding = UseDenseCase | UseSparseCase

type LiteralIds = M.Map (Either BcConst Id) Word16

viewCaseAlts ::
     CaseType
  -> [(BcTag, a, label)]
  -> (Maybe label, [(BcTag, a, label)], UseCaseEncoding, Int)
     -- ^ Returns: (1) default case, (2) cases, (3) which encoding to
     -- use (4) the bytecode length when using that encoding
viewCaseAlts CaseOnTag alts0 =
  if dense_case_len <= sparse_case_len then
    (dflt, alts, UseDenseCase, dense_case_len)
  else
    (dflt, alts, UseSparseCase, sparse_case_len)

 where
   alts1 = sortBy (comparing fst3) alts0
   (dflt, alts)
     | ((DefaultTag,_,d):r) <- alts1 = (Just d, r)
     | otherwise                     = (Nothing, alts1)
   Tag max_tag = fst3 (last alts)

   -- compact CASE starts with tag 1
   dense_case_len = 1 + ceilDiv2 max_tag

   -- sparse CASE has custom min and max
   sparse_case_len = 2 + length alts
   ceilDiv2 x = (x + 1) `div` 2
   fst3 (t,_,_) = t


putLinearIns :: LiteralIds
             -> NewAddresses
             -> Int
             -> FinalIns
             -> InsBuildM ()
putLinearIns lit_ids new_addrs ins_id ins = case ins of
  -- TODO: We need to record the offset of each instruction (for branches)
  -- TODO: Where to put liveness info in CASE instructions?
  Lst Stop ->
    putIns (insAD opc_STOP 0 0)
  Lst (Ret1 (BcReg x _)) ->
    putIns (insAD opc_RET1 (i2b x) 0)
  Lst (Eval _ lives (BcReg r _))
    | Just bitset <- regsToBits (S.delete (BcReg r VoidTy) lives) -> do
    putIns (insAD opc_EVAL (i2b r) 0)
    putIns bitset
    putIns (insAD opc_MOV_RES (i2b r) 0)
  Lst (Call Nothing (BcReg f _) args) ->
    assert (args == map (\n -> BcReg n VoidTy) [0 .. length args - 1]) $
    putIns (insAD opc_CALLT (i2b f) (i2h (length args)))
  Lst (Call (Just (BcReg rslt _, _, lives)) (BcReg f _) (BcReg arg0 _:args))
    | Just bitset <- regsToBits (S.delete (BcReg rslt VoidTy) lives) -> do
      putIns (insABC opc_CALL (i2b f) (i2b $ 1 + length args) (i2b arg0))
      putArgs args
      putIns bitset
      putIns (insAD opc_MOV_RES (i2b rslt) 0)
  Lst (Case casetype x alts) ->
    putCase ins_id casetype x alts new_addrs
  Lst (Goto tgt) ->
    putIns $ insAJ opc_JMP 0 (new_addrs IM.! tgt - new_addrs IM.! ins_id - 1)
  Lst Update ->
    putIns (insAD opc_UPDATE 0 1)
  Lst (CondBranch cond ty (BcReg r1 _) (BcReg r2 _) t1 t2)
   | ty == IntTy || ty == CharTy
   -> do
    let (swap_targets, target)
           | t1 == ins_id + 1 = (True, t2)
           | t2 == ins_id + 1 = (False, t1)
           | otherwise = error "putLinearIns: CondBranch: No branch target adjacent"
        cond' | swap_targets = invertCondition cond
              | otherwise    = cond
        condOpcode c = case c of
          CmpGt -> opc_ISGT
          CmpLe -> opc_ISLE
          CmpGe -> opc_ISGE
          CmpLt -> opc_ISLT
          CmpEq -> opc_ISEQ
          CmpNe -> opc_ISNE
        next_ins_addr = (new_addrs IM.! ins_id) + 2
        offs = (new_addrs IM.! target) - next_ins_addr
    putIns $ insAD (condOpcode cond') (i2b r1) (i2h r2)
    putIns $ insAJ opc_JMP 0 offs
  Mid (Assign (BcReg d _) (Move (BcReg s _))) | d == s ->
    return () -- redundant move instruction
  Mid (Assign (BcReg d _) (Move (BcReg s _))) ->
    putIns $ insAD opc_MOV (i2b d) (i2h s)
  Mid (Assign (BcReg d _) (BinOp op ty (BcReg a _) (BcReg b _))) ->
    putIns $ insABC (binOpOpcode ty op) (i2b d) (i2b a) (i2b b)
  Mid (Assign (BcReg d _) (Load (LoadGlobal x))) ->
    putIns $ insAD opc_LOADK (i2b d) (lit_ids M.! Right x)
  Mid (Assign (BcReg d _) (Load (LoadLit l))) ->
    putIns $ insAD opc_LOADK (i2b d) (lit_ids M.! Left l)
  Mid (Assign (BcReg d _) (Load LoadBlackhole)) ->
    putIns $ insAD opc_LOADBH (i2b d) 0
  Mid (Assign (BcReg d _) (Load (LoadClosureVar idx))) ->
    putIns $ insAD opc_LOADFV (i2b d) (i2h idx)
  Mid (Assign (BcReg d _) (Load LoadSelf)) ->
    putIns $ insAD opc_LOADSLF (i2b d) 0
  Mid (Assign (BcReg d _) (Alloc (BcReg i _) args lives))
    | Just bitset <- regsToBits lives -> do
    case args of
      [BcReg a _] ->
        putIns $ insABC opc_ALLOC1 (i2b d) (i2b i) (i2b a)
      _ -> do
        putIns $ insABC opc_ALLOC (i2b d) (i2b i) (i2b (length args))
        putArgs args
    putIns bitset
  Mid (Assign (BcReg d _) (AllocAp (BcReg a0 _:args) lives))
    | Just bitset <- regsToBits lives -> do
    putIns $ insABC opc_ALLOCAP (i2b d) (i2b a0) (i2b (length args))
    putArgs args
    putIns bitset
  Mid (Assign (BcReg d _) (Fetch (BcReg n _) fld)) ->
    putIns $ insABC opc_LOADF (i2b d) (i2b n) (i2b fld)
  Mid (Store (BcReg ptr _) offs (BcReg src _)) | offs <= 255 ->
    putIns $ insABC opc_INITF (i2b ptr) (i2b src) (i2b offs)
  Mid m -> error $ pretty m

 where
   binOpOpcode :: OpTy -> BinOp -> Word8
   binOpOpcode IntTy OpAdd = opc_ADDRR
   binOpOpcode IntTy OpSub = opc_SUBRR
   binOpOpcode IntTy OpMul = opc_MULRR
   binOpOpcode IntTy OpDiv = opc_DIVRR
   binOpOpcode IntTy OpRem = opc_REMRR

-- | Encode a case instruction.
--
-- Assumes that the default case is fall-through.
--
-- Chooses between dense and sparse case based on which
-- encoding is smaller.
--
-- In a dense encoding, @num_cases == max_tag + 1@.  If
-- a tag is not present in the alternatives, we generate a branch
-- to the default case (i.e., fall-through.)
--
-- In a sparse encoding we have a pair of tag and target.
--
putCase :: Int -> CaseType -> BcVar -> [(BcTag, a, Int)]
        -> NewAddresses
        -> InsBuildM ()
putCase this_id casetype (BcReg r _) alts0 new_addrs =
  case enc of
    UseDenseCase -> do
      putIns $ insAD opc_CASE (i2b r) (fromIntegral (length alts))
      putDenseAlts
    UseSparseCase -> do
      putIns $ insAD opc_CASE_S (i2b r) (fromIntegral (length alts))
      putMinMax
      putSparseAlts

 where
   (dflt, alts, enc, len) = viewCaseAlts casetype alts0

   dflt_addr = new_addrs IM.! this_id + len

   putDenseAlts = putWord16s $ go 1 alts
    where
      go _ [] = []
      go next_tag alts0@((Tag tag',_,tgt):alts') =
        if next_tag == tag' then
          fromIntegral (new_addrs IM.! tgt - dflt_addr) :
            go (next_tag + 1) alts'
         else
          0 : go (next_tag + 1) alts0

   putSparseAlts =
     forM_ alts $ \(Tag tag, _, tgt) ->
       putWord16s [ fromIntegral tag,
                    fromIntegral (new_addrs IM.! tgt - dflt_addr) ]

   putMinMax =
     let (Tag min_tag, _, _) = head alts
         (Tag max_tag, _, _) = last alts
     in putWord16s [ i2h min_tag, i2h max_tag ]

-- | Output 'Word16's as multiple of 'Word32's (LSB first).
--
-- > putWord16s [0x1234, 0x5678, 0x9abc]
-- > -- output: [0x56781234, 0x00009abc]
putWord16s :: [Word16] -> InsBuildM ()
putWord16s wds = go wds
  where
    go []     = return ()
    go (w:ws) = go' (h2w w) ws
    go' acc []     = putIns acc
    go' acc (w:ws) = putIns (acc .|. (h2w w `shiftL` 16)) >> go ws

data InsState = InsState (IM.IntMap Int)  -- length of each instr
                         !Int

newtype InsBuildM a = InsBuildM (StateT InsState BuildM a)
  deriving (Monad)

putIns :: Word32 -> InsBuildM ()
putIns w = InsBuildM $ do
  lift $ emit $ fromWrite $ writeWord32be w
  modify (\(InsState m i) -> InsState m (i + 1))

recordLength :: Int -> InsBuildM a -> InsBuildM a
recordLength n act = do
  InsState m x <- InsBuildM get
  InsBuildM $ put $! InsState m 0
  r <- act
  InsState m' len <- InsBuildM get
  InsBuildM $ put $! InsState (IM.insert n len m') x
  return r

getLenghts :: InsBuildM (IM.IntMap Int)
getLenghts = do
  InsState m x <- InsBuildM get
  return m

runInsBuildM :: InsBuildM a -> BuildM a
runInsBuildM (InsBuildM sm) = evalStateT sm (InsState IM.empty 0)

-- A call like
--
-- > putArgs (map BcReg [1..6])
--
-- outputs
--
-- > 0x04030201 0x00000605
--
-- I.e., it outputs multiples of 'Word32's where the LSB represents
-- the first argument, the next byte the second argument, etc.
putArgs :: [BcVar] -> InsBuildM ()
putArgs regs_ =
  putWord8s [ i2b r | reg <- regs_, let BcReg r _ = reg ]

putWord8s :: [Word8] -> InsBuildM ()
putWord8s ws = go 0 ws 0
 where
   go !shift [] !acc
     | shift == 0 = return ()
     | otherwise  = putIns acc
   go !shift bs0@(b:bs) !acc
     | shift < 32 =
       go (shift + 8) bs (acc .|. (b2w b `shiftL` shift))
     | otherwise =
       putIns acc >> go 0 bs0 0

test_putArgs =
  let (_, _, b) =  runBuildM $ runInsBuildM $
                     putArgs (map (\n -> BcReg n VoidTy) [1..6]) in
  L.unpack (toLazyByteString b) == [4,3,2,1,0,0,6,5]

--putCase

b2w :: Word8 -> Word32
b2w = fromIntegral

b2h :: Word8 -> Word16
b2h = fromIntegral

h2w :: Word16 -> Word32
h2w = fromIntegral
w2h :: Word32 -> Word16
w2h = fromIntegral
i2h :: Int -> Word16
i2h = fromIntegral

i2b :: Int -> Word8
i2b = fromIntegral

insABC :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
insABC o a b c =
  b2w o .|. (b2w a `shiftL` 8) .|. (b2w c `shiftL` 16)
        .|. (b2w b `shiftL` 24)

insAD :: Word8 -> Word8 -> Word16 -> Word32
insAD o a d =
  b2w o .|. (b2w a `shiftL` 8) .|. (h2w d `shiftL` 16)


insAJ :: Word8 -> Word8 -> Int -> Word32
insAJ o a j =
  b2w o .|. (b2w a `shiftL` 8) .|. (j' `shiftL` 16)
 where
   j' = fromIntegral (j + br_bias) :: Word32

-- | Turn set of registers into bitmask.  Returns 'Nothing' if
-- more than 31 bits would be required.
regsToBits :: S.Set BcVar -> Maybe Word32
regsToBits regs_ = go 0 (S.toList regs_)
 where
   go !bitset [] = Just bitset
   go !bitset (BcReg n _ : rs)
     | n > 30 = Nothing
     | otherwise = go (bitset .|. (1 `shiftL` n)) rs

----------------------------------------------------------------------

-- | Turn set of register into bitmask.
--
-- The most significant bit of each 'Word16' is non-zero if there are
-- more words to follow.
regsToBits16 :: S.Set BcVar -> [Word16]
regsToBits16 regs_ = go 0 0 (S.toList regs_)
 where
   go !bitset !offset [] = [bitset]
   go !bitset !offset rs@(BcReg n _ : rs')
     | n - offset > 15 =
       (0x8000 .|. bitset) : go 0 (offset + 15) rs
     | otherwise =
       go (bitset .|. (1 `shiftL` (n - offset))) offset rs'

test_regsToBits16 =
  regsToBits16
    (S.fromList [BcReg 5 VoidTy, BcReg 13 VoidTy, BcReg 17 VoidTy])
    == [40992,4]

bitsToWord32s :: [Bool] -> [Word32]
bitsToWord32s bits_ = go 0 1 bits_
 where
   go :: Word32 -> Word32 -> [Bool] -> [Word32]
   go !acc !mask []
     | mask == 1 = []
     | otherwise  = [acc]
--   go !acc !mask bits@(b:bs)
--     | trace (show (acc, mask, bits)) False = undefined
   go !acc !mask bits@(b:bs)
     | mask == 0 = -- mask overflowed
       acc : go 0 1 bits
     | otherwise  =
       let !acc' | b         = acc .|. mask
                 | otherwise = acc
       in go acc' (mask `shiftL` 1) bs

test_bitsToWord32s =
  [bitsToWord32s [True, False] == [1],
   bitsToWord32s (replicate 32 False ++ [True]) == [0, 1],
   bitsToWord32s [] == []]

addString_ :: B.ByteString -> Build Word
addString_ s = Build $ \tbl n k ->
  case M.lookup s tbl of
    Just m -> k tbl n m
    Nothing -> let !m = n + 1 in k (M.insert s n tbl) m n

getStringTable_ :: Build (M.Map B.ByteString Word)
getStringTable_ = Build $ \tbl n k -> k tbl n tbl

liftBuildM :: R.BuildM a -> Build a
liftBuildM bm = Build $ \tbl n k -> bm >>= k tbl n

emitWord8s :: R.Region -> [Word8] -> Build ()
emitWord8s r = liftBuildM . R.emitWord8s r

emitVarUInt :: R.Region -> Word -> Build ()
emitVarUInt r = liftBuildM . R.emitWord8s r . varUIntBytes

emitVarSInt :: R.Region -> Int -> Build ()
emitVarSInt r = emitVarUInt r . zigZagEncode

emitWord32be :: R.Region -> Word32 -> Build ()
emitWord32be r = liftBuildM . R.emitWord32be r

emitId :: R.Region -> Id -> Build ()
emitId r the_id =
  emitIdString r (show the_id ++ type_suffix the_id)
 where
    type_suffix anId = case idDetails anId of
      TopLevelId -> "`closure"
      InfoTableId -> "`info"
      DataConId -> "`con"
      DataConInfoTableId -> "`con_info"
      _ -> "`other"

-- | Emit a fully quantified identifier.
--
-- The string is encoded as a length-prefixed list of string references.
-- For example the string @\"Foo.Bar.blub\"@ is in a string table
--
-- > { 1: "Foo", 23: "Bar", 42: "blub" }
--
-- is encoded as the bytes
--
-- > [3, 1, 23, 42]
--
-- the leading @3@ is the number of components.  All parts use varint
-- encoding, so some references may consist of multiple bytes.
--
emitIdString :: R.Region -> String -> Build ()
emitIdString r str = emitId' r (B.split dot (U.fromString str))
 where
   dot = fromIntegral (ord '.') :: Word8

-- | Emit an identifier made up of the given components.
emitId' :: R.Region -> [B.ByteString] -> Build ()
emitId' r parts = do
  let n = length parts
  emitVarUInt r (fromIntegral n)
  forM_ parts $ \p -> do
    idx <- addString_ p
    emitVarUInt r idx

emitWord32sbe :: R.Region -> [Word32] -> Build ()
emitWord32sbe r [] = return ()
emitWord32sbe r (w:ws) = emitWord32be r w >> emitWord32sbe r ws

-- | Encode a pointer bitmap for use by the garbage collector.
-- Prefixed by the total size of the object (length of
-- the input list).
emitPointerMask :: R.Region -> [OpTy] -> Build ()
emitPointerMask r ops = do
  emitVarUInt r (fromIntegral (length ops))
  emitWord32sbe r (bitsToWord32s (map isGCPointer ops))

emitInsAD :: R.Region -> Word8 -> Word8 -> Word16 -> Build ()
emitInsAD r opc a d =
  emitWord32be r (b2w opc .|. (b2w a `shiftL` 8) .|. (h2w d `shiftL` 16))

emitInsABC :: R.Region -> Word8 -> Word8 -> Word8 -> Word8 -> Build ()
emitInsABC r opc a b c =
  emitWord32be r $!
    b2w opc .|. (b2w a `shiftL` 8) .|. (b2w c `shiftL` 16)
            .|. (b2w b `shiftL` 24)

emitInsAJ :: R.Region -> Word8 -> Word8 -> R.Label -> Build ()
emitInsAJ r opc a target = do
  liftBuildM (R.reference' R.S2NoRC R.BE offs_to_j r target)
  liftBuildM (R.emitWord16be r (b2h opc .|. (b2h a `shiftL` 8)))
 where
   offs_to_j offs = (offs `shiftR` 2) - 1 + br_bias
     -- "- 1" because in bytecode jmp 0 is a nop, not a loop, i.e.,
     -- offsets are relative to the next instruction

type TargetLabels = IM.IntMap R.Label

collectLabels :: FinalCode -> [Int]
collectLabels code = V.foldr f [] (fc_code code)
 where
   f :: (LinearIns' Int) -> [Int] -> [Int]
   f (Lst (Case _ _ alts))            rst = map thd3 alts ++ rst
   f (Lst (Goto l))                   rst = l : rst
   f (Lst (CondBranch _ _ _ _ l1 l2)) rst = l1 : l2 : rst
   f _                                rst = rst

mkTargetLabels :: FinalCode -> Build TargetLabels
mkTargetLabels code =
  IM.fromList <$> mapM mkLbl (collectLabels code)
 where
   mkLbl i = (,) i <$> liftBuildM R.makeLabel

-- | Emit the bit sets corresponding to the given 'LiveSet'.
--
-- We actually store the bit set in a different region and just emit
-- an offset to that location in the code.  In the region we emit
-- first the pointer information followed by the liveness info.  The
-- reasoning is that GC info is needed more frequently than JIT
-- compilation info.
--
emitBitSets :: R.Region -- ^ Bitset region
            -> LiveSet  -- ^ Liveset to be emitted
            -> R.Region -- ^ Target region
            -> Build ()
emitBitSets rbitsets lives r = do
  let livebits = regsToBits16 lives
  let pointerbits = regsToBits16 (S.filter isPtrReg lives)
  l <- liftBuildM (R.label rbitsets)
  emitWord16s rbitsets pointerbits
  emitWord16s rbitsets livebits
  liftBuildM (R.reference R.S4 R.BE r l)

-- | Does this register contain a pointer.
isPtrReg :: BcVar -> Bool
isPtrReg (BcReg _ t) = isGCPointer t

emitWord16s :: R.Region -> [Word16] -> Build ()
emitWord16s _ []     = return ()
emitWord16s r (w:ws) = liftBuildM (R.emitWord16be r w) >> emitWord16s r ws

emitLinearIns :: R.Region -- ^ Region containing bit masks.
              -> LiteralIds
              -> TargetLabels
              -> R.Region -- ^ Region for instruction.
              -> Int
              -> FinalIns
              -> Build ()
emitLinearIns bit_r lit_ids tgt_labels r ins_id ins = do
  case IM.lookup ins_id tgt_labels of
    Just l  -> liftBuildM (R.placeLabel r l)
    Nothing -> return ()
  case ins of
    Lst Stop ->
      emitInsAD r opc_STOP 0 0
    Lst (Ret1 (BcReg x _)) ->
      emitInsAD r opc_RET1 (i2b x) 0
    Lst (Eval _ lives (BcReg reg _)) -> do
      emitInsAD r opc_EVAL (i2b reg) 0
      emitBitSets bit_r (S.delete (BcReg reg VoidTy) lives) r
      emitInsAD r opc_MOV_RES (i2b reg) 0
    Lst (Call Nothing (BcReg f _) args) ->
      assert (length args <= cMAX_CALL_ARGS) $ do
      assert (args == map (\n -> BcReg n VoidTy) [0 .. length args - 1]) $ do
      let [ptrs] = bitsToWord32s (map isPtrReg args)
      emitInsABC r opc_CALLT (i2b f) (fromIntegral ptrs) (i2b (length args))
      --emitBitSets bit_r (S.fromList args) r
    Lst (Call (Just (BcReg rslt _, _, lives)) (BcReg f _) args)      -> do
        let [ptrs] = bitsToWord32s (map isPtrReg args)
        emitInsABC r opc_CALL (i2b f) (fromIntegral ptrs) (i2b $ length args)
        emitArgs r args
        emitBitSets bit_r (S.delete (BcReg rslt VoidTy) lives) r
        emitInsAD r opc_MOV_RES (i2b rslt) 0
    Lst (Case casetype x alts) ->
      emitCase r casetype x alts tgt_labels
    Lst (Goto tgt) ->
      emitInsAJ r opc_JMP 0 (tgt_labels IM.! tgt)
    Lst Update ->
      emitInsAD r opc_UPDATE 0 1
    Lst (CondBranch cond ty (BcReg r1 _) (BcReg r2 _) t1 t2)
     | ty == IntTy || ty == CharTy
     -> do
      let (swap_targets, target)
             | t1 == ins_id + 1 = (True, t2)
             | t2 == ins_id + 1 = (False, t1)
             | otherwise =
               error "emitLinearIns: CondBranch: No branch target adjacent"
          cond' | swap_targets = invertCondition cond
                | otherwise    = cond
          condOpcode c = case c of
            CmpGt -> opc_ISGT
            CmpLe -> opc_ISLE
            CmpGe -> opc_ISGE
            CmpLt -> opc_ISLT
            CmpEq -> opc_ISEQ
            CmpNe -> opc_ISNE
      emitInsAD r (condOpcode cond') (i2b r1) (i2h r2)
      emitInsAJ r opc_JMP 0 (tgt_labels IM.! target)
    Mid (Assign (BcReg d _) (Move (BcReg s _))) | d == s ->
      return () -- redundant move instruction
    Mid (Assign (BcReg d _) (Move (BcReg s _))) ->
      emitInsAD r opc_MOV (i2b d) (i2h s)
    Mid (Assign (BcReg d _) (BinOp op ty (BcReg a _) (BcReg b _))) ->
      emitInsABC r (binOpOpcode ty op) (i2b d) (i2b a) (i2b b)
    Mid (Assign (BcReg d _) (Load (LoadGlobal x))) ->
      emitInsAD r opc_LOADK (i2b d) (lit_ids M.! Right x)
    Mid (Assign (BcReg d _) (Load (LoadLit l))) ->
      emitInsAD r opc_LOADK (i2b d) (lit_ids M.! Left l)
    Mid (Assign (BcReg d _) (Load LoadBlackhole)) ->
      emitInsAD r opc_LOADBH (i2b d) 0
    Mid (Assign (BcReg d _) (Load (LoadClosureVar idx))) ->
      emitInsAD r opc_LOADFV (i2b d) (i2h idx)
    Mid (Assign (BcReg d _) (Load LoadSelf)) ->
      emitInsAD r opc_LOADSLF (i2b d) 0
    Mid (Assign (BcReg d _) (Alloc (BcReg i _) args lives)) -> do
      case args of
        [BcReg a _] ->
          emitInsABC r opc_ALLOC1 (i2b d) (i2b i) (i2b a)
        _ -> do
          emitInsABC r opc_ALLOC (i2b d) (i2b i) (i2b (length args))
          emitArgs r args
      emitBitSets bit_r lives r
    Mid (Assign (BcReg d _) (AllocAp args lives)) -> 
      assert (length args - 1 <= cMAX_CALL_ARGS) $ do
      -- the pointer mask excludes the first argument (because it's
      -- always a pointer)
      let [ptrs] = bitsToWord32s (map isPtrReg (tail args))
      emitInsABC r opc_ALLOCAP (i2b d) (fromIntegral ptrs) (i2b (length args - 1))
      emitArgs r args
      emitBitSets bit_r lives r
    Mid (Assign (BcReg d _) (Fetch (BcReg n _) fld)) ->
      emitInsABC r opc_LOADF (i2b d) (i2b n) (i2b fld)
    Mid (Store (BcReg ptr _) offs (BcReg src _)) | offs <= 255 ->
      emitInsABC r opc_INITF (i2b ptr) (i2b src) (i2b offs)
    Mid m -> error $ pretty m

 where
   binOpOpcode :: OpTy -> BinOp -> Word8
   binOpOpcode IntTy OpAdd = opc_ADDRR
   binOpOpcode IntTy OpSub = opc_SUBRR
   binOpOpcode IntTy OpMul = opc_MULRR
   binOpOpcode IntTy OpDiv = opc_DIVRR
   binOpOpcode IntTy OpRem = opc_REMRR

-- | A call like
--
-- > putArgs (map BcReg [1..6])
--
-- outputs the 'Word32's
--
-- > 0x04030201 0x00000605
--
-- I.e., it outputs multiples of 'Word32's where the LSB represents
-- the first argument, the next byte the second argument, etc.
emitArgs :: R.Region -> [BcVar] -> Build ()
emitArgs rgn regs_ =
  emitWord32sbe rgn $
    word8sToWord32s [ i2b r | reg <- regs_, let BcReg r _ = reg ]

emitCase :: R.Region -> CaseType -> BcVar -> [(BcTag, a, Int)]
         -> TargetLabels -> Build ()
emitCase r casetype (BcReg reg _) alts0 tgt_labels = do
  dflt_label <- liftBuildM R.makeLabel
  case enc of
    UseDenseCase -> do
      emitInsAD r opc_CASE (i2b reg) (fromIntegral (length alts))
      emitDenseAlts dflt_label
      liftBuildM $ R.placeLabel r dflt_label

    UseSparseCase -> do
      emitInsAD r opc_CASE_S (i2b reg) (fromIntegral (length alts))
      emitMinMax
      emitSparseAlts dflt_label
 where
   (dflt, alts, enc, len) = viewCaseAlts casetype alts0

   -- Tags that don't occur in the case alternatives just point to
   -- the default branch
   emitDenseAlts dflt_label =
     mapMWord16s $ go 1 alts
    where
      go _ [] = []
      go next_tag alts0@((Tag tag',_,tgt):alts') =
        if next_tag == tag' then
          liftBuildM (R.offset' R.S2 R.BE (`shiftR` 2) r
                                dflt_label (tgt_labels IM.! tgt))
            : go (next_tag + 1) alts'
         else
          liftBuildM (R.emitWord16be r 0) : go (next_tag + 1) alts0

   -- input builds must write a single Word16 in big endian each
   mapMWord16s :: [Build ()] -> Build ()
   mapMWord16s [] = return ()
   mapMWord16s [b] = liftBuildM (R.emitWord16be r 0) >> b
   mapMWord16s (b1:b2:bs) = b2 >> b1 >> mapMWord16s bs

   emitSparseAlts dflt_label =
     forM_ alts $ \(Tag tag, _, tgt) -> do
       liftBuildM (R.emitWord16be r (fromIntegral tag))
       liftBuildM (R.offset' R.S2 R.BE (`shiftR` 2) r
                             dflt_label (tgt_labels IM.! tgt))

   emitMinMax =
     let (Tag min_tag, _, _) = head alts
         (Tag max_tag, _, _) = last alts
     in emitWord32sbe r (word16sToWord32s [ i2h min_tag, i2h max_tag ])

word8sToWord32s :: [Word8] -> [Word32]
word8sToWord32s ws = go 0 ws 0
 where
   go !shift [] !acc
     | shift == 0 = []
     | otherwise  = [acc]
   go !shift bs0@(b:bs) !acc
     | shift < 32 =
       go (shift + 8) bs (acc .|. (b2w b `shiftL` shift))
     | otherwise = acc : go 0 bs0 0

word16sToWord32s :: [Word16] -> [Word32]
word16sToWord32s wds = go wds
 where
   go []     = []
   go (w:ws) = go' (h2w w) ws
   go' !acc []     = [acc]
   go' !acc (w:ws) = (acc .|. (h2w w `shiftL` 16)) : go ws

test_word16sToWord32s =
  [ word16sToWord32s [] == [],
    word16sToWord32s [1, 3] == [ 0x00030001 ],
    word16sToWord32s [1, 2, 3] == [ 0x00020001, 0x3 ]
  ]

encodeModule' :: BytecodeModule -> L.ByteString
encodeModule' mdl =
  R.toLazyByteString id $ do
    -- regions (must be in the order in which they appear in the output)
    rheader <- R.newRegion
    rmodinfo <- R.newRegion

    (_, _) <- runBuild $ do
      emitIdString rmodinfo (bcm_name mdl)
      mapM_ (emitIdString rmodinfo) imports

      magic rmodinfo "BCCL"
      nitbls <- sum <$> (forM (M.toList bcos) $ \(name, bco) ->
                          emitInfoTable name bco)

      rclosures <- liftR R.newRegion
      nclosures <- sum <$> (forM (M.toList bcos) $ \(name, bco) ->
                             emitClosure rclosures name bco)
      s <- getStringTable'
      liftR $ R.emitLazyByteString rheader $
        header s nitbls nclosures

      return ()
    return ()
 where
   liftR :: R.BuildM a -> Build a
   liftR = liftBuildM

   bcos = bcm_bcos mdl
   imports = bcm_imports mdl

   -- A special four character marker
   magic :: R.Region -> String -> Build ()
   magic r str@[_,_,_,_] =
     liftR $ R.emitWord8s r (map (i2b . ord) str)

   -- returns number of info tables emitted (0 or 1)
   emitInfoTable :: Id -> BytecodeObject' FinalCode
                   -> Build Word
   emitInfoTable name bco =
     case bco of
       BcConInfo tag fields tys -> do
         r <- liftR R.newRegion
         magic r "ITBL"
         emitId r name
         emitVarUInt r cltype_CONSTR
         emitVarUInt r (fromIntegral tag)
         assert (fields == length tys) $ do
         emitPointerMask r tys
         emitId r name
         return 1
       BcObject{ bcoType = ty }
         | BcoFun arity arg_tys <- ty -> do
            emitInfoTable_ (mkInfoTableId (idName name)) cltype_FUN
                           arity arg_tys bco
         | ty == Thunk -> do
            emitInfoTable_ (mkInfoTableId (idName name)) cltype_THUNK
                           0 [] bco
         | ty == CAF -> do
            emitInfoTable_ (mkInfoTableId (idName name)) cltype_CAF
                           0 [] bco

       BcoCon{ } ->
         return 0

       BcTyConInfo{ } -> return 0

   --emitInfoTable_ :: Id -> Word8 -> Int -> BCO -> ...
   emitInfoTable_ name cltype arity arg_tys bco = do
     r <- liftR R.newRegion
     magic r "ITBL"
     emitId r name
     emitVarUInt r cltype
     emitPointerMask r (map snd (M.toList (bcoFreeVars bco)))
     emitId r name
     emitCode arity arg_tys (bcoCode bco)
     return 1


   -- Create the closure part for a BCO
   -- Returns number of closure definitions emitted (0 or 1)
   emitClosure :: R.Region -> Id -> BytecodeObject' FinalCode
               -> Build Word
   emitClosure r name bco =
     case bco of
       BcoCon _ con_id fields -> do
         magic r "CLOS"
         emitId r name
         emitVarUInt r (fromIntegral (length fields))
         emitId r con_id
         mapM_ (emitField r) fields
         return 1
       BcObject{ bcoType = BcoFun arity _, bcoFreeVars = fvs }
         | M.size fvs == 0 -> do
           magic r "CLOS"
           emitId r name
           emitVarUInt r 0  -- no payload
           emitId r (mkInfoTableId (idName name)) -- info table
           -- no payload, hence no literals
           return 1
         | otherwise ->
           -- A function with free variables need not have a static
           -- closure.
           return 0
       BcObject{ bcoType = CAF, bcoFreeVars = fvs } | M.size fvs == 0 -> do
         magic r "CLOS"
         emitId r name
         emitVarUInt r 2  -- one word for the indirection, one for linking
         emitId r (mkInfoTableId (idName name))  -- info table
         mapM_ (emitField r) [Left (CInt 0), Left (CInt 0)]
         return 1
       BcObject{ bcoType = Thunk } ->
         return 0  -- don't need a static closure
       BcTyConInfo{ } ->
         return 0
       BcConInfo _ _ _ -> return 0
       _ ->
         error $ "UNIMPL: encodeClosure: " ++ pretty bco


   emitField :: R.Region -> Either BcConst Id -> Build ()
   emitField r lit = case lit of
     Left (CInt n) -> do
       emitVarUInt r littype_INT
       emitVarSInt r (fromIntegral n)
     Left (CStr s) -> do
       emitVarUInt r littype_STRING
       sid <- addString_ (U.fromString s)
       emitVarUInt r sid
     Left (CChar c) -> do
       emitVarUInt r littype_CHAR
       emitVarUInt r (fromIntegral (ord c))
     Left (CWord n) -> do
       emitVarUInt r littype_WORD
       emitVarUInt r (fromIntegral n)
     Left (CFloat f) -> do
       emitVarUInt r littype_FLOAT
       emitWord32be r (floatToWord32 $ fromRational f)
     Right x -> do
       case idDetails x of
         InfoTableId -> emitVarUInt r littype_INFO
         DataConInfoTableId -> emitVarUInt r littype_INFO
         _ -> emitVarUInt r littype_CLOSURE
       emitId r x

   emitCode :: Int -> [OpTy] -> FinalCode -> Build ()
   emitCode arity arg_tys code = 
     assert (arity == length arg_tys) $ do

     r <- liftR R.newRegion
     rcode <- liftR R.newRegion
     rbitsets <- liftR R.newRegion

     code_start <- liftR R.makeLabel
     code_end <- liftR R.makeLabel
     bitset_start <- liftR R.makeLabel
     bitset_end <- liftR R.makeLabel
     liftR $ R.placeLabel rcode code_start
     liftR $ R.placeLabel rbitsets bitset_start

     emitVarUInt r (fromIntegral (fc_framesize code))
     emitVarUInt r (fromIntegral arity)
     let literals = collectLiterals code
         lit_ids = M.fromAscList (zip (S.toList literals) [0..])
     emitVarUInt r (fromIntegral (M.size lit_ids))
     liftR $ R.offset' R.S2 R.BE (`shiftR` 2) r code_start code_end
     liftR $ R.offset' R.S2 R.BE (`shiftR` 1) r bitset_start bitset_end
     emitLiterals r lit_ids

     -- If arity > 0, then the first bitmap is the function pointer bitmap
     case arg_tys of
       [] -> return ()
       tys -> do
         let ptr_arg_regs =
               S.fromList [ BcReg n t | (n, t) <- zip [0..] tys,
                                        isGCPointer t ]
         emitWord16s rbitsets (regsToBits16 ptr_arg_regs)
     
     emitInsAD rcode opc_FUNC (i2b (fc_framesize code)) 0
     emitInstructions rbitsets rcode lit_ids code
     liftR $ R.placeLabel rcode code_end
     liftR $ R.padTo rbitsets 4 0
     liftR $ R.placeLabel rbitsets bitset_end
     return ()
     --putLinearIns lit_ids (Lst Stop)  -- bytecode dummy

   emitLiterals r lit_ids = do
     forM_ (M.keys lit_ids) (emitField r)

   header strings numItbls numClosures =
     toLazyByteString $ mconcat
     [ fromString "KHCB" -- magic
     , fromWrite (writeWord16be 0 `mappend` writeWord16be 1) -- version
     , fromWrite (writeWord32be 0) -- flags
     , fromWrite (writeWord32be (fromIntegral (M.size strings)))
     , fromWrite (writeWord32be (fromIntegral numItbls))
     , fromWrite (writeWord32be (fromIntegral numClosures))
     , fromWrite (writeWord32be (fromIntegral (length imports)))
     , fromString "BCST" -- string table section magic
     , encodeStringTable strings
     --, mdl_outp
--     , fromString "BCCL" -- closure table section magic
     ]

adjustCodeOffset :: Int -> Int
adjustCodeOffset n =
  trace ("adjustCodeOffset:" ++ show n) $ n `shiftR` 2

emitInstructions :: R.Region -> R.Region -> LiteralIds -> FinalCode
                 -> Build ()
emitInstructions rbitmasks r lit_ids code = do
  tgt_labels <- mkTargetLabels code
  --trace ("tgts " ++ show tgt_labels) $ do
  let inss = zip [0..] (V.toList (fc_code code))
  forM_ inss $ \(ins_id, ins) -> do
    emitLinearIns rbitmasks lit_ids tgt_labels r ins_id ins

assertEqualLBS :: L.ByteString -> L.ByteString -> a -> a
assertEqualLBS b1_ b2_ kont = go 0 b1_ b2_
 where
   go !n xs0 ys0
     | Just (x,xs) <- L.uncons xs0,
       Just (y,ys) <- L.uncons ys0,
       x == y
     = go (n + 1) xs ys
     | L.null xs0 && L.null ys0
     = kont
     | otherwise
     = error $ "strings don't match at offset " ++ show n ++ "\n" ++
        show (max (n - 32) 0) ++ "-" ++ show (n + 32) ++ ":\n" ++
        show (L.unpack (L.take 64 (L.drop (n - 32) b1_))) ++ "\n" ++
        show (L.unpack (L.take 64 (L.drop (n - 32) b2_)))


