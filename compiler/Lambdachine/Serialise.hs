{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Lambdachine.Serialise where

import Lambdachine.Id
import Lambdachine.Grin.Bytecode
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
import Control.Exception.Base ( assert )
import Control.Monad ( liftM, ap, forM_, forM )
import Control.Monad.State.Strict --( StateT, runStateT )
import Data.Bits
import Data.Char ( ord )
import Data.List ( sortBy, find )
import Data.Monoid
import Data.Ord ( comparing )
import Data.Word
import Debug.Trace ( trace )

#include "../Opcodes.h"

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

encodeModule :: BytecodeModule -> L.ByteString
encodeModule mdl = toLazyByteString builder
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
       BcConInfo tag fields ty -> do
         emit $ fromString "ITBL"
         encodeId name
         emit $ varUInt cltype_CONSTR
         emit $ varUInt (fromIntegral tag)
         emit $ varUInt (fromIntegral fields) -- TODO: ptrs
         emit $ varUInt 0 -- TODO: nptrs
         encodeId name
         return 1
       BcObject{ bcoType = ty }
         | BcoFun arity <- ty -> do
            let itblName = mkInfoTableId (idName name)
            emit $ fromString "ITBL"
            encodeId itblName
            emit $ varUInt cltype_FUN
            emit $ varUInt 0 -- TODO: ptrs
            emit $ varUInt 0 -- TODO: nptrs
            encodeId itblName
            encodeCode arity (bcoCode bco)
            return 1
         | ty `elem` [Thunk, CAF] -> do
            let itblName = mkInfoTableId (idName name)
            emit $ fromString "ITBL"
            encodeId itblName
            emit $ varUInt cltype_THUNK
            emit $ varUInt (i2w (bcoFreeVars bco)) -- TODO: ptrs
            emit $ varUInt 0 -- TODO: nptrs
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
       BcObject{ bcoType = BcoFun arity } -> do
         emit $ fromString "CLOS"
         encodeId name
         emit $ varUInt 0  -- no payload
         encodeId (mkInfoTableId (idName name)) -- info table
         -- no payload, hence no literals
         return 1
       BcObject{ bcoType = CAF, bcoFreeVars = fvs } | fvs == 0 -> do
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
     emit $ varUInt (i2w (codesize + 1))
     encodeLiterals lit_ids
     emit $ fromWrite $ writeWord32be $
       insAD opc_FUNC (i2b (fc_framesize code)) 0
     encodeInstructions lit_ids code
     return ()
     --putLinearIns lit_ids (Lst Stop)  -- bytecode dummy
     
   encodeLiterals lit_ids = do
     forM_ (M.keys lit_ids) encodeField
   
encodeId :: Id -> BuildM ()
encodeId the_id =
  encodeIdString (show the_id ++ type_suffix the_id)
 where
    type_suffix anId = case idDetails anId of
      TopLevelId -> "!closure"
      InfoTableId -> "!info"
      DataConId -> "!con"
      DataConInfoTableId -> "!con_info"
      _ -> "!other"

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

-- IMPORTANT: must match implementation of putLinearIns
insLength :: FinalIns -> Int
insLength ins = 
  let l = insLength' ins in l
--  trace ("insLength " ++ pretty ins ++ " => " ++ show l) l
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
  Mid (Assign _ (Alloc _ args@(x1:x2:_))) -> 1 + arg_len args
  Mid (Assign _ (AllocAp (_:args))) -> 1 + arg_len args
  Mid _ -> 1
 where
   ceilDiv4 x = (x + 3) `div` 4
   arg_len args = ceilDiv4 (length args)

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
  Lst (Ret1 (BcReg x)) ->
    putIns (insAD opc_RET1 (i2b x) 0)
  Lst (Eval _ lives (BcReg r))
    | Just bitset <- regsToBits lives -> do
    putIns (insAD opc_EVAL (i2b r) 0)
    putIns bitset
    putIns (insAD opc_MOV_RES (i2b r) 0)
  Lst (Call Nothing (BcReg f) args) ->
    assert (args == map BcReg [0 .. length args - 1]) $
    putIns (insAD opc_CALLT (i2b f) (i2h (length args)))
  Lst (Call (Just (BcReg rslt, _, lives)) (BcReg f) (BcReg arg0:args)) 
    | Just bitset <- regsToBits lives -> do
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
  Lst (CondBranch cond ty (BcReg r1) (BcReg r2) t1 t2)
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
  Mid (Assign (BcReg d) (Move (BcReg s))) | d == s ->
    return () -- redundant move instruction
  Mid (Assign (BcReg d) (Move (BcReg s))) ->
    putIns $ insAD opc_MOV (i2b d) (i2h s)
  Mid (Assign (BcReg d) (BinOp op ty (BcReg a) (BcReg b))) ->
    putIns $ insABC (binOpOpcode ty op) (i2b d) (i2b a) (i2b b)
  Mid (Assign (BcReg d) (Load (LoadGlobal x))) ->
    putIns $ insAD opc_LOADK (i2b d) (lit_ids M.! Right x)
  Mid (Assign (BcReg d) (Load (LoadLit l))) ->
    putIns $ insAD opc_LOADK (i2b d) (lit_ids M.! Left l)
  Mid (Assign (BcReg d) (Load LoadBlackhole)) ->
    putIns $ insAD opc_LOADBH (i2b d) 0
  Mid (Assign (BcReg d) (Load (LoadClosureVar idx))) ->
    putIns $ insAD opc_LOADFV (i2b d) (i2h idx)
  Mid (Assign (BcReg d) (Load LoadSelf)) ->
    putIns $ insAD opc_LOADSLF (i2b d) 0
  Mid (Assign (BcReg d) (Alloc (BcReg i) args)) ->
    case args of
      [BcReg a] -> 
        putIns $ insABC opc_ALLOC1 (i2b d) (i2b i) (i2b a)
      _ -> do
        putIns $ insABC opc_ALLOC (i2b d) (i2b i) (i2b (length args))
        putArgs args
  Mid (Assign (BcReg d) (AllocAp (BcReg a0:args))) -> do
    putIns $ insABC opc_ALLOCAP (i2b d) (i2b a0) (i2b (length args))
    putArgs args
  Mid (Assign (BcReg d) (Fetch (BcReg n) fld)) ->
    putIns $ insABC opc_LOADF (i2b d) (i2b n) (i2b fld)
  Mid (Store (BcReg ptr) offs (BcReg src)) | offs <= 255 ->
    putIns $ insABC opc_INITF (i2b ptr) (i2b src) (i2b offs)
  Mid m -> error $ pretty m
  
 where
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
putCase this_id casetype (BcReg r) alts0 new_addrs =
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
  putWord8s [ i2b r | reg <- regs_, let BcReg r = reg ]

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
  let (_, _, b) =  runBuildM $ runInsBuildM $ putArgs (map BcReg [1..6]) in
  L.unpack (toLazyByteString b) == [4,3,2,1,0,0,6,5]

--putCase 

b2w :: Word8 -> Word32
b2w = fromIntegral

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
   go !bitset (BcReg n : rs)
     | n > 30 = Nothing
     | otherwise = go (bitset .|. (1 `shiftL` n)) rs
