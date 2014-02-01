{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, MultiParamTypeClasses,
             FlexibleInstances, CPP #-}
module Lambdachine.Grin.Bytecode
  ( module Lambdachine.Grin.Bytecode,
    (<*>), (|*><*|), O, C, emptyGraph, catGraphs, MaybeO(..),
    withFresh, HooplNode(..), freshLabel, UniqueMonad(..), Label,
  )
where

import Lambdachine.Id
import Lambdachine.Utils.Pretty
import Lambdachine.Utils ( second, snd3 )
import qualified Lambdachine.Utils.Unique as U

import qualified Type as Ghc
import qualified Outputable as Ghc
import qualified DynFlags as Ghc

import Compiler.Hoopl
import Control.Monad.State
import Control.DeepSeq
import Data.Maybe ( maybeToList, fromMaybe )
--import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate.Direct
import Data.Bits ( (.&.) )
import qualified Data.Vector as V
import Data.Binary

#include "../../Opcodes.h"

instance Pretty Ghc.Type where
  ppr t = withGlobalEnv $ \env ->
            text $ Ghc.showSDoc (envDynFlags env) $ Ghc.ppr t

type BlockId = Label

type LiveSet = S.Set BcVar

instance NFData Label

-- | A bytecode instruction.  Suitable for use with Hoopl.
data BcIns' b e x where
  Label  :: !b -> BcIns' b C O
  -- O/O stuff
  Assign :: !BcVar -> !BcRhs         -> BcIns' b O O
  Store  :: !BcVar -> !Int -> !BcVar -> BcIns' b O O

  -- O/C stuff
  Goto   :: !b                       -> BcIns' b O C
  CondBranch :: !CmpOp -> !OpTy -> !BcVar -> !BcVar
             -> !b -> !b             -> BcIns' b O C
  Case :: !CaseType -> !BcVar 
       -> [(BcTag, LiveSet, b)]   -> BcIns' b O C
  Call :: !(Maybe (BcVar, b, LiveSet))
       -> !BcVar -> ![BcVar]         -> BcIns' b O C
  Ret1 :: !BcVar                     -> BcIns' b O C
  RetN :: ![BcVar]                   -> BcIns' b O C
  Eval   :: !b -> !LiveSet -> !BcVar -> BcIns' b O C
  -- only used by the interpreter / RTS
  Update ::                             BcIns' b O C
  Stop   ::                             BcIns' b O C

-- | A linearised bytecode instruction.
data LinearIns' b
  = Fst (BcIns' b C O)
  | Mid (BcIns' b O O)
  | Lst (BcIns' b O C)

type LinearIns = LinearIns' BlockId
type BcIns = BcIns' Label

instance Biplate (LinearIns' x) BcVar where
  biplate (Fst ins) = plate Fst |+ ins
  biplate (Mid ins) = plate Mid |+ ins
  biplate (Lst ins) = plate Lst |+ ins

instance Pretty b => Pretty (LinearIns' b) where
  ppr (Fst (Label l)) = text "---" <+> ppr l <+> text "---"
  ppr (Mid i) = ppr i
  ppr (Lst i) = ppr i

instance NFData b => NFData (BcIns' b e x) -- where
--  rnf (Case t v targets) = rnf targets `seq` ()  -- FIXME: complete this instance

data CaseType
  = CaseOnTag !Int  -- no. of tags
  | CaseOnLiteral
  deriving (Eq, Ord)

instance NFData CaseType

data BcTag
  = DefaultTag
  | Tag !Int
  | LitT !Integer
  deriving (Eq, Ord, Show)

instance NFData BcTag

data BcRhs
  = Move !BcVar
  | HiResult !Int   -- for loading results of a multi-result return
  | Load !BcLoadOperand
  | BinOp !BinOp !OpTy !BcVar !BcVar
  | Fetch !BcVar Int
  | Alloc !BcVar ![BcVar] !LiveSet
  | AllocAp ![BcVar] !LiveSet
  | PrimOp !PrimOp !OpTy ![BcVar]
  deriving (Eq, Ord)

data BcLoadOperand
  = LoadLit !BcConst
  | LoadGlobal !Id
  | LoadClosureVar Int  -- must be lazy
  | LoadSelf
  | LoadBlackhole
  deriving (Eq, Ord)

instance NFData BcLoadOperand where
  rnf (LoadClosureVar !idx) = ()

{-
data CompOp
  = CmpGt | CmpLe | CmpGe | CmpLt | CmpEq | CmpNe
  deriving (Eq, Ord)
-}
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpRem
  -- Comparison operators that return 0 or 1
  | CmpGtI | CmpLeI | CmpGeI | CmpLtI | CmpEqI | CmpNeI
  deriving (Eq, Ord, Show)

data CmpOp
  = CmpGt | CmpLe | CmpGe | CmpLt | CmpEq | CmpNe

data PrimOp
  = OpIndexOffAddrChar
  | OpGetTag
  | OpNegateInt
  | OpBitNot
  | OpBitAnd
  | OpBitOr
  | OpBitXor
  | OpShiftLeft
  | OpShiftRightLogical
  | OpShiftRightArith
  | OpRaise  -- TODO: Just stops the program, for now.
  | OpNop  -- See Note "Primitive Nops"
  deriving (Eq, Ord, Show)

-- Note: Primitive Nops
-- --------------------
--
-- Some type changing primitive operations are actually a no-op at
-- runtime.  For example, ord#, chr#, int2word#, etc. fall into this
-- category because Char# and Int# and Word# are all represented as
-- machine words.  Converting one into the other only requires
-- re-interpreting the same bits.  We currently map them all into the
-- single `OpNop` prim-op.  At some point in the future we may want to
-- do some type checking to verify the compiler's doings.  At that
-- point, having more type information may be useful.


data OpTy = IntTy
          | WordTy
          | Int64Ty
          | Word64Ty
          | CharTy
          | FloatTy
          | DoubleTy
          | AddrTy
          | PtrTy
          | FunTy ![OpTy] !OpTy
          | AlgTy !Id
          | VoidTy
  deriving (Eq, Ord)

instance NFData OpTy where
  rnf (FunTy args _) = rnf args `seq` ()
  rnf x = x `seq` ()

data BcVar = BcVar !Id Ghc.Type
           | BcReg {-# UNPACK #-} !Int OpTy

instance NFData BcVar where
  rnf (BcVar _ !t) = ()
  rnf (BcReg _ !t) = ()

-- | Comparison of 'BcVar' ignores type differences.
compareBcVar :: BcVar -> BcVar -> Ordering
compareBcVar (BcVar n _) (BcVar m _) = compare n m
compareBcVar (BcVar _ _) _           = LT
compareBcVar (BcReg r _) (BcReg s _) = compare r s
compareBcVar (BcReg _ _) (BcVar _ _) = GT

instance Eq BcVar where
  v == q = compareBcVar v q == EQ

instance Ord BcVar where
  compare = compareBcVar

-- instance Show BcVar where show v = pretty v

instance U.Uniquable BcVar where
  getUnique (BcVar x _) = U.getUnique x
  getUnique (BcReg n _) = U.unsafeMkUniqueNS 'R' n

instance NonLocal (BcIns' Label) where
  entryLabel (Label l) = l
  successors (Goto l) = [l]
  successors (CondBranch _ _ _ _ tl fl) = [fl, tl]
  successors (Case _ _ targets) = map (\(_,_,t) -> t) targets
  successors (Call mb_l _ _) = maybeToList (snd3 `fmap` mb_l)
  successors (Ret1 _) = []
  successors (RetN _) = []
  successors (Eval l _ _) = [l]

instance HooplNode BcIns where
  mkBranchNode l = Goto l
  mkLabelNode l = Label l

hooplUniqueFromUniqueSupply :: U.Supply U.Unique -> Unique
hooplUniqueFromUniqueSupply us =
  intToUnique ((U.intFromUnique (U.supplyValue us)) .&. 0xffffff)

-- -------------------------------------------------------------------

instance Pretty BcVar where
  ppr (BcVar v t) = ppr v <> colour2 (brackets (ppr t))
  ppr (BcReg n t) = char 'r' <> int n <> char '_' <> ppr t
  
instance Pretty Label where
  ppr lbl = text (show lbl)

--instance Pretty CompOp where

pprLives :: LiveSet -> PDoc
pprLives lives = braces (hsep (commaSep (map ppr (S.toList lives))))

instance Pretty BinOp where
  ppr OpAdd = char '+'
  ppr OpSub = char '-'
  ppr OpMul = char '*'
  ppr OpDiv = char '/'
  ppr OpRem = char '%'
  ppr CmpGtI = text ">#"
  ppr CmpLeI = text "<=#"
  ppr CmpGeI = text ">=#"
  ppr CmpLtI = text "<#"
  ppr CmpEqI = text "==#"
  ppr CmpNeI = text "/=#"

instance Pretty CmpOp where
  ppr CmpGt = char '>'
  ppr CmpLe = text "<="
  ppr CmpGe = text ">="
  ppr CmpLt = char '<'
  ppr CmpEq = text "=="
  ppr CmpNe = text "/="

instance Pretty b => Pretty (BcIns' b e x) where
  ppr (Label _) = empty
  ppr (Assign d (Move s)) | d == s = text "---"
  ppr (Assign r rhs) = ppr r <+> char '=' <+> ppr rhs
  ppr (Eval _ lives r) =
    text "eval" <+> ppr r <+> pprLives lives
  ppr (Store base offs val) =
    text "Mem[" <> ppr base <+> char '+' <+> int offs <> text "] = " <> ppr val
  ppr (Goto bid) = text "goto" <+> ppr bid
  ppr (CondBranch cmp ty r1 r2 true false) =
    text "if" <+> ppr r1 <+> ppr cmp <+> ppr r2 <+>
      char '<' <> ppr ty <> char '>' <+> text "then goto" <+> ppr true <+>
      brackets (text "else goto" <+> ppr false)
  ppr (Case _ r targets) =
    text "case" <+> ppr r $$
    indent 2 (vcat (map ppr_target targets))
   where
     ppr_target (tag, lives, target) = 
       ppr tag <> colon <+> ppr target <+> pprLives lives
  ppr (Call rslt f args) =
    align $
      (case rslt of
         Nothing -> text "return"
         Just (r,_,_) -> ppr r <+> char '=') <+>
      ppr f <> parens (hsep (commaSep (map ppr args)))
  ppr (Ret1 r) = text "return" <+> ppr r
  ppr (RetN rs) =
    text "return" <+> parens (hsep (commaSep (map ppr rs)))
  ppr Update = text "update"
  ppr Stop = text "stop"

instance Pretty BcRhs where
  ppr (Move r) = ppr r
  ppr (Load op) = ppr op
  ppr (BinOp op ty src1 src2) =
    ppr src1 <+> ppr op <+> ppr src2 <+> char '<' <> (ppr ty) <> char '>'
  ppr (Fetch r offs) =
    text "Mem[" <> ppr r <+> char '+' <+> int offs <> char ']'
  ppr (Alloc ctor args lives) =
    text "alloc(" <> hsep (commaSep (map ppr (ctor:args))) <> char ')'
      <+> pprLives lives
  ppr (AllocAp args lives) =
    text "alloc_ap(" <> hsep (commaSep (map ppr args)) <> char ')'
      <+> pprLives lives
  ppr (HiResult n) =
    text "result(" <> ppr n <> char ')'
  ppr (PrimOp op _ty args) =
    ppr op <> char '(' <> hsep (commaSep (map ppr args)) <> char ')'

instance Pretty PrimOp where
  ppr OpIndexOffAddrChar = text "indexCharOffAddr#"
  ppr OpGetTag = text "getTag#"
  ppr OpNegateInt = text "negateInt#"
  ppr OpBitNot = text "not#"
  ppr OpBitAnd = text "and#"
  ppr OpBitOr = text "or#"
  ppr OpBitXor = text "xor#"
  ppr OpShiftLeft = text "shiftLeft#"
  ppr OpShiftRightLogical = text "shiftRightLogical#"
  ppr OpShiftRightArith = text "shiftRightArith#"
  ppr OpRaise = text "raise#"
  ppr OpNop = text "nop"

instance Pretty OpTy where
  ppr VoidTy = text "v"
  ppr IntTy = text "i"
  ppr Int64Ty = text "I"
  ppr WordTy = text "u"
  ppr Word64Ty = text "U"
  ppr CharTy = text "c"
  ppr AddrTy = text "a"
  ppr FloatTy = text "f"
  ppr DoubleTy = text "d"
  ppr PtrTy = text "p"
  ppr (FunTy args res) =
    char '(' <> hcat (map ppr args) <> text "):" <> ppr res
  ppr (AlgTy n) =
    char '<' <> ppr n <> char '>'

instance Pretty BcLoadOperand where
  ppr (LoadLit l) = ppr l
  ppr (LoadGlobal x) = char '&' <> ppr x
  ppr (LoadClosureVar n) = text "Node[" <> int n <> char ']'
  ppr LoadSelf = text "Node"
  ppr LoadBlackhole = text "<blackhole>"

instance Pretty BcTag where
  ppr DefaultTag = text "_"
  ppr (Tag n) = int n
  ppr (LitT n) = char '#' <> text (show n)

{-
tst1 = do
  pprint ((Assign (BcReg 1 IntTy) (BinOp OpAdd IntTy (BcReg 2) (BcReg 3))) :: BcIns O O)
  pprint ((Assign (BcReg 2) (Fetch (BcReg 2) 42)) :: BcIns O O)
-}
-- -------------------------------------------------------------------

mapLabels :: (l1 -> l2) -> BcIns' l1 e x -> BcIns' l2 e x
mapLabels f ins = case ins of
  Label l      -> Label (f l)
  Assign x rhs -> Assign x rhs
  Eval l lv x     -> Eval (f l) lv x
  Store x n y  -> Store x n y
  Ret1 x       -> Ret1 x
  RetN xs      -> RetN xs
  Goto l       -> Goto (f l)
  CondBranch op ty x y l1 l2 ->
    CondBranch op ty x y (f l1) (f l2)
  Case cty x targets ->
    Case cty x (map (\(x,y,z) -> (x, y, f z)) targets)
  Call next fn args ->
    Call (fmap (\(x,y,z) -> (x, f y, z)) next) fn args

type NodePpr n = forall e x . n e x -> PDoc
type LabelPpr = Label -> PDoc

pprGraph :: NodePpr n -> LabelPpr -> Graph n e x -> PDoc
pprGraph ppN _ GNil = text "{}"
pprGraph ppN _ (GUnit blk) =
    char '{' $$ pprBlock ppN blk $$ char '}'
pprGraph ppN ppL (GMany entry blocks exit) =
    char '{' $$
      indent 2 (ppMaybeO pp_block_oc entry $$
                vcat pp_blocks $$
                ppMaybeO (pprBlock ppN) exit) $$
    char '}'
   where
     pp_block_oc blk =
       indent 3 (pprBlock ppN blk)
     pp_blocks = map pp_block (mapToList blocks)
     pp_block (l, blk) =
       ppL l $$ indent 3 (pprBlock ppN blk)

pprBlock :: NodePpr n -> Block n e x -> PDoc
pprBlock ppN blk =
  --ppMaybeC ppN entry $$
  vcat (map ppN (blockToList middles)) $$
  ppMaybeC ppN exit
 where (entry, middles, exit) = blockSplitAny blk

ppMaybeO :: (a -> PDoc) -> MaybeO o a -> PDoc
ppMaybeO pp NothingO = empty
ppMaybeO pp (JustO a) = pp a

ppMaybeC :: (a -> PDoc) -> MaybeC o a -> PDoc
ppMaybeC pp NothingC = empty
ppMaybeC pp (JustC a) = pp a

{-
pprBlock' :: forall n e x.
             NodePpr n -> Block n e x -> IndexedCO x PDoc PDoc
pprBlock' ppN block =
  foldBlockNodesF f block empty
 where
   f :: n e1 x1 -> PDoc -> PDoc
   f n d = d $$ ppN n
-}

type BcGraph e x = AGraph BcIns e x

finaliseBcGraph :: UniqueMonad m => BcGraph O C -> m (Graph BcIns O C)
finaliseBcGraph agr = graphOfAGraph agr

mkLabel :: Label -> BcGraph C O
mkLabel l = mkFirst $ Label l

insBinOp :: BinOp -> OpTy -> BcVar -> BcVar -> BcVar -> BcGraph O O
insBinOp op ty rslt src1 src2 =
  mkMiddle $ Assign rslt (BinOp op ty src1 src2)

insPrimOp :: PrimOp -> OpTy -> BcVar -> [BcVar] -> BcGraph O O
insPrimOp op ty rslt args =
  mkMiddle $ Assign rslt (PrimOp op ty args)

insLoadLit :: BcVar -> BcConst -> BcGraph O O
insLoadLit r lit = mkMiddle $ Assign r (Load (LoadLit lit))

insLoadGbl :: BcVar -> Id -> BcGraph O O
insLoadGbl r gbl = mkMiddle $ Assign r (Load (LoadGlobal gbl))

insLoadFV :: BcVar -> Int -> BcGraph O O
insLoadFV r n = mkMiddle $ Assign r (Load (LoadClosureVar n))

insLoadSelf :: BcVar -> BcGraph O O
insLoadSelf r = mkMiddle $ Assign r (Load LoadSelf)

insLoadBlackhole :: BcVar -> BcGraph O O
insLoadBlackhole r = mkMiddle $ Assign r (Load LoadBlackhole)

insMkAp :: BcVar -> [BcVar] -> BcGraph O O
insMkAp r args 
 | length args > cMAX_CALL_ARGS + 1 = error "Too many arguments to ALLOCAP"
 | otherwise
 = mkMiddle $ Assign r (AllocAp args S.empty)

insMove :: BcVar -> BcVar -> BcGraph O O
insMove dst src = mkMiddle $ Assign dst (Move src)

insLoadExtraResult :: BcVar -> Int -> BcGraph O O
insLoadExtraResult dst n = mkMiddle $ Assign dst (HiResult n)

insAlloc :: BcVar -> BcVar -> [BcVar] -> BcGraph O O
insAlloc r dcon args = mkMiddle $ Assign r (Alloc dcon args S.empty)

insStore :: BcVar -> Int -> BcVar -> BcGraph O O
insStore base offs val = mkMiddle $ Store base offs val

insFetch :: BcVar -> BcVar -> Int -> BcGraph O O
insFetch dst base offs = mkMiddle $ Assign dst (Fetch base offs)

insEval :: BlockId -> BcVar -> BcGraph O C
insEval b r = mkLast $ Eval b S.empty r

insRet1 :: BcVar -> BcGraph O C
insRet1 r = mkLast $ Ret1 r

insRetN :: [BcVar] -> BcGraph O C
insRetN results = mkLast (RetN results)

insCase :: CaseType -> BcVar -> [(BcTag, BlockId)] -> BcGraph O C
insCase cty r targets =
  mkLast $ Case cty r (map (\(t, b) -> (t, S.empty, b)) targets)

insBranch :: CmpOp -> OpTy -> BcVar -> BcVar -> BlockId -> BlockId
          -> BcGraph O C
insBranch op ty r1 r2 ltrue lfalse =
  mkLast $ CondBranch op ty r1 r2 ltrue lfalse

insGoto :: BlockId -> BcGraph O C
insGoto l = mkLast $ Goto l

insCall :: Maybe (BcVar, BlockId) -> BcVar -> [BcVar] -> BcGraph O C
insCall kont f args 
  | length args > cMAX_CALL_ARGS
  = error $ "Too many arguments to CALL/CALLT (" ++ show (length args) ++ ")"
  | otherwise
  = mkLast $ Call kont' f args
  where kont' = do (x, l) <- kont
                   return (x, l, S.empty)


catGraphsC :: BcGraph e C -> [BcGraph C C] -> BcGraph e C
catGraphsC g [] = g
catGraphsC g (h:hs) = catGraphsC (g |*><*| h) hs

data BytecodeObject' g
  = BcObject
    { bcoType :: BcoType
    , bcoCode :: g
    , bcoGlobalRefs :: [Id]
    , bcoConstants :: [BcConst]
    , bcoFreeVars  :: M.Map Int OpTy
    }
  | BcoCon
    { bcoType :: BcoType -- ^ Always 'Con'.  Only for completeness.
    , bcoDataCon :: Id   -- ^ The constructor 'Id'.
    , bcoFields :: [Either BcConst Id]
    }
  | BcConInfo
    { bcoConTag :: Int
    , bcoConFields :: Int
    , bcoConArgTypes :: [OpTy]
    }
  | BcTyConInfo
    { bcoDataCons :: [Id] }

type BytecodeObject = BytecodeObject' (Graph BcIns O C)

type BCOs = M.Map Id BytecodeObject
type FinalBCOs = M.Map Id (BytecodeObject' FinalCode)

data BytecodeModule = BytecodeModule
  { bcm_name    :: String   -- TODO: use better type
  , bcm_imports :: [String] -- TODO: use better type
  , bcm_bcos :: FinalBCOs
  }

instance Pretty BytecodeModule where
  ppr mdl =
    angleBrackets $
    sep [text (bcm_name mdl),
         brackets (fillSep (commaSep (map text (bcm_imports mdl)))),
         ppr (bcm_bcos mdl)]

data BcoType
  = BcoFun Int [OpTy]  -- arity and argument types
  | Thunk
  | CAF
  | Con
  deriving Eq

bcoArity :: BytecodeObject' g -> Int
bcoArity BcObject{ bcoType = BcoFun n _ } = n
bcoArity _ = 0


collectLiterals :: FinalCode -> S.Set (Either BcConst Id)
collectLiterals code = V.foldl' comb S.empty (fc_code code)
 where
   comb :: S.Set (Either BcConst Id) -> FinalIns
        -> S.Set (Either BcConst Id)
   comb s (Fst _) = s
   comb s (Mid ins_) = case ins_ of
     Assign _ (Load (LoadLit c)) -> S.insert (Left c) s 
     Assign _ (Load (LoadGlobal x)) -> S.insert (Right x) s
     _ -> s
   comb s (Lst _) = s

data BcConst
  = CInt Integer
  | CStr String
  | CChar Char
  | CWord Integer
  | CInt64 Integer
  | CWord64 Integer
  | CFloat Rational
  | CDouble Rational
  deriving (Eq, Ord, Show)

-- | A linearised and register-allocated version of the byte code.
-- Jump offsets are relative (@0@ = next instruction) and explicit
-- labels have been removed.
data FinalCode = FinalCode
  { fc_framesize :: {-# UNPACK #-} !Int
    -- ^ Maximum number of registers used by the bytecode.
  , fc_code      :: V.Vector (LinearIns' Int)
  }

type FinalIns = LinearIns' Int

instance Pretty BcConst where
  ppr (CInt n) = ppr n
  ppr (CStr s) = text (show s)
  ppr (CWord n) = text (show n) <> char 'u'
  ppr (CChar c) = text (show c)
  ppr (CInt64 n) = text (show n) <> char 'L'
  ppr (CWord64 n) = text (show n) <> char 'U'
  ppr (CFloat r) = text (show (fromRational r :: Float)) <> char 'f'
  ppr (CDouble r) = text (show (fromRational r :: Double)) <> char 'd'

instance Pretty BcoType where
  ppr (BcoFun n ts) =
    text "FUN_" <> int n <> char '_' <> hcat (map ppr ts)
  ppr Thunk = text "THUNK"
  ppr CAF = text "CAF"
  ppr Con = text "CON"

instance Pretty g => Pretty (BytecodeObject' g) where
  ppr bco@BcObject{} =
    align $ ppr (bcoType bco) <> char ':' <>
              int (M.size (bcoFreeVars bco)) $+$
            text "gbl: " <> align (ppr (bcoGlobalRefs bco)) $+$
            (indent 2 $ ppr (bcoCode bco))
             -- pprGraph ppr (\l -> ppr l <> colon) (bcoCode bco))
  ppr BcoCon{ bcoDataCon = dcon, bcoFields = fields } =
     ppr dcon <+> hsep (map pp_fld fields)
    where pp_fld (Left l) = ppr l
          pp_fld (Right x) = ppr x
  ppr BcConInfo{ bcoConTag = tag, bcoConArgTypes = tys } =
    text "CONSTR" <>
      (parens $ align $
       sep [text "tag:" <> ppr tag <> comma,
            text "args:" <> sep (commaSep (map ppr tys))])
  ppr BcTyConInfo{ bcoDataCons = dcons } =
    text "TYPE" <> parens (hsep (commaSep (map ppr dcons)))

instance Pretty (Graph BcIns O C) where
  ppr g = pprGraph ppr (\l -> ppr l <> colon) g

instance Pretty FinalCode where
  ppr (FinalCode framesize code) =
    text "    alloc_frame" <+> ppr framesize $+$
    vcat (map pp (zip [(0::Int)..] (V.toList code)))
   where
     pp (i, ins) = fillBreak 3 (ppr i) <+> ppr ins

------------------------------------------------------------------------
-- * Optimisation Stuff
{-
class PrettyNode n where
  pprNode :: n e x -> PDoc

newtype BcM a = BcM (State Int a)
  deriving Monad

runBcM :: BcM a -> a
runBcM (BcM s) = evalState s 0

instance UniqueMonad BcM where
  freshUnique = BcM $ do
    s <- get
    let !u = intToUnique s
    put $! s + 1
    return u

instance FuelMonad BcM where
  getFuel = return infiniteFuel
  setFuel _ = return ()

instance CheckpointMonad BcM where
  type Checkpoint BcM = Int
  checkpoint = BcM $ get
  restart us = BcM $ put us
{-
tst0 = putStrLn $ runBcM $ do
  l <- freshLabel
  let g0 = mkFirst (Label l) <*>
           mkMiddles [Assign (Var 1) (Var 2),
                      Store (Var 0) 5 (Var 1),
                      Assign (Var 3) (Var 2)
                     ] <*>
           mkLast (Goto l)
  (g1, lives1, _) <- analyzeAndRewriteBwd
                      livenessAnalysis1 (JustC l)
                      g0 noFacts
  (g2, lives2, _) <- analyzeAndRewriteBwd
                      livenessAnalysis2 (JustC l)
                      g0 noFacts
  return $
    showGraph showNode g1 ++ "\n" ++ 
    show (mapToList lives1) ++ "\n---------------\n" ++
    showGraph showNode g2 ++ "\n" ++ 
    show (mapToList lives2)
-}
tst2 = pprint $ runBcM $ do
  l <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  let g0 = (mkFirst (Label l) <*>
            mkMiddles [Assign (BcReg 1) (Move (BcReg 2)),
                       Assign (BcReg 2) (Load (LoadLit (CInt 1))),
                      Store (BcReg 0) 5 (BcReg 1),
                      Assign (BcReg 3) (Move (BcReg 2))
                     ] <*>
            mkLast (Goto l2)) |*><*|
           (mkFirst (Label l2) <*>
            mkMiddles [Assign (BcReg 1) (BinOp OpAdd IntTy (BcReg 1) (BcReg 0)),
                       Assign (BcReg 0) (BinOp OpSub IntTy (BcReg 0) (BcReg 2))] <*>
            mkLast (CondBranch CmpGt IntTy (BcReg 1) (BcReg 0) l2 l3))
           |*><*|
           (mkFirst (Label l3) <*> mkLast (Ret1 (BcReg 1)))
  (g1, lives1, _) <- analyzeAndRewriteBwd livenessAnalysis2 (JustC l)
                       g0 noFacts
  return $
    pprGraph ppr (ppL lives1) g1
 where
  ppL lives l = ppr l <> colon <+> text "--- lives=" <> ppr (fromMaybe S.empty (lookupFact l lives))

tst3 :: [BcVar]
tst3 = universeBi $ Assign (BcReg 1) (BinOp OpAdd IntTy (BcReg 1) (BcReg 0))

--newtype BcGraph e x = BcGraph (Graph BcIns e x)

-- -------------------------------------------------------------------
-}
instance Uniplate BcVar where uniplate p = plate p

instance Biplate (BcIns' b e x) BcVar where
  biplate (Assign r rhs) = plate Assign |* r |+ rhs
  biplate (Eval l lv r) = plate (Eval l) |+ lv |* r
  biplate (Store r n r') = plate Store |* r |- n |* r'
  biplate (CondBranch c t r1 r2 l1 l2) =
    plate (CondBranch c t) |* r1 |* r2 |- l1 |- l2
  biplate (Case ty r targets) =
    plate (Case ty) |* r ||+ targets
  biplate (Call Nothing f args) =
    plate (Call Nothing) |* f ||* args
  biplate (Call (Just (x,y,lives)) f args) =
    plate (\x' lives' -> Call (Just (x', y, lives'))) |* x |+ lives |* f ||* args
  biplate (Ret1 r) = plate Ret1 |* r
  biplate (RetN rs) = plate RetN ||* rs
  biplate l = plate l

instance Biplate (S.Set BcVar) BcVar where
  biplate vars = plate S.fromList ||* S.toList vars

instance Biplate (BcTag, S.Set BcVar, b) BcVar where
  biplate (t,vars,b) =
    plate (\vars' -> (t, S.fromList vars', b)) ||* S.toList vars

instance Biplate BcRhs BcVar where
  biplate (Move r) = plate Move |* r
  biplate (BinOp op ty r1 r2) = plate (BinOp op ty) |* r1 |* r2
  biplate (Fetch r n) = plate Fetch |* r |- n
  biplate (Alloc rt rs lv) = plate Alloc |* rt ||* rs |+ lv
  biplate (AllocAp rs lv) = plate AllocAp ||* rs |+ lv
  biplate (PrimOp op ty vars) = plate (PrimOp op ty) ||* vars
  biplate rhs@(Load _) = plate rhs
  biplate rhs@(HiResult _) = plate rhs

invertCondition :: CmpOp -> CmpOp
invertCondition cond = case cond of
  CmpGt -> CmpLe
  CmpLe -> CmpGt
  CmpGe -> CmpLt
  CmpLt -> CmpGe
  CmpEq -> CmpNe
  CmpNe -> CmpEq

-- | Is this type represented as a pointer that needs to be followed
-- by the GC?
isGCPointer :: OpTy -> Bool
isGCPointer (FunTy _ _) = True
isGCPointer (AlgTy _)   = True
isGCPointer PtrTy       = True
isGCPointer _           = False
