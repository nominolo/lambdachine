{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, MultiParamTypeClasses,
             FlexibleInstances #-}
module Lambdachine.Grin.Bytecode
  ( module Lambdachine.Grin.Bytecode,
    (<*>), (|*><*|), O, C, emptyGraph, catGraphs, MaybeO(..),
    withFresh, HooplNode(..), freshLabel, UniqueMonad(..),
  )
where

import Lambdachine.Id
import Lambdachine.Utils.Pretty
import Lambdachine.Utils ( second, snd3 )
import qualified Lambdachine.Utils.Unique as U

import Compiler.Hoopl
import Control.Monad.State
import Data.Maybe ( maybeToList, fromMaybe )
--import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Generics.Uniplate.Direct
import Data.Bits ( (.&.) )
import qualified Data.Vector as V
import Data.Binary

type BlockId = Label

data BcIns' b e x where
  Label  :: b -> BcIns' b C O
  -- O/O stuff
  Assign :: BcVar -> BcRhs        -> BcIns' b O O
  Store  :: BcVar -> Int -> BcVar -> BcIns' b O O

  -- O/C stuff
  Goto   :: b                     -> BcIns' b O C
  CondBranch :: BinOp -> OpTy -> BcVar -> BcVar
             -> b -> b            -> BcIns' b O C
  Case :: CaseType -> BcVar 
       -> [(BcTag, S.Set BcVar, b)]            -> BcIns' b O C
  Call :: Maybe (BcVar, b, S.Set BcVar)
       -> BcVar -> [BcVar]        -> BcIns' b O C
  Ret1 :: BcVar                   -> BcIns' b O C
  Eval   :: b -> S.Set BcVar -> BcVar            -> BcIns' b O C
  -- only used by the interpreter / RTS
  Update ::                          BcIns' b O C
  Stop   ::                          BcIns' b O C

data LinearIns' b
  = Fst (BcIns' b C O)
  | Mid (BcIns' b O O)
  | Lst (BcIns' b O C)

type LinearIns = LinearIns' BlockId
type BcIns = BcIns' Label

instance Biplate LinearIns BcVar where
  biplate (Fst ins) = plate Fst |+ ins
  biplate (Mid ins) = plate Mid |+ ins
  biplate (Lst ins) = plate Lst |+ ins

instance Pretty b => Pretty (LinearIns' b) where
  ppr (Fst (Label l)) = text "---" <+> ppr l <+> text "---"
  ppr (Mid i) = ppr i
  ppr (Lst i) = ppr i

data CaseType
  = CaseOnTag
  | CaseOnLiteral
  deriving (Eq, Ord)

data BcTag
  = DefaultTag
  | Tag Int
  | LitT Integer
  deriving (Eq, Ord, Show)

data BcRhs
  = Move BcVar
  | Load BcLoadOperand
  | BinOp BinOp OpTy BcVar BcVar
  | Fetch BcVar Int
  | Alloc BcVar [BcVar]
  | AllocAp [BcVar]
  deriving (Eq, Ord)

data BcLoadOperand
  = LoadLit BcConst
  | LoadGlobal Id
  | LoadClosureVar Int
  | LoadSelf
  | LoadBlackhole
  deriving (Eq, Ord)
{-
data CompOp
  = CmpGt | CmpLe | CmpGe | CmpLt | CmpEq | CmpNe
  deriving (Eq, Ord)
-}
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv
  | CmpGt | CmpLe | CmpGe | CmpLt | CmpEq | CmpNe
  deriving (Eq, Ord, Show)

data OpTy = Int32Ty | Float32Ty
  deriving (Eq, Ord)

data BcVar = BcVar !Id
           | BcReg {-# UNPACK #-} !Int
  deriving (Eq, Ord)

instance Show BcVar where show v = pretty v

instance NonLocal (BcIns' Label) where
  entryLabel (Label l) = l
  successors (Goto l) = [l]
  successors (CondBranch _ _ _ _ tl fl) = [fl, tl]
  successors (Case _ _ targets) = map (\(_,_,t) -> t) targets
  successors (Call mb_l _ _) = maybeToList (snd3 `fmap` mb_l)
  successors (Ret1 _) = []
  successors (Eval l _ _) = [l]

instance HooplNode BcIns where
  mkBranchNode l = Goto l
  mkLabelNode l = Label l

hooplUniqueFromUniqueSupply :: U.Supply U.Unique -> Unique
hooplUniqueFromUniqueSupply us =
  intToUnique ((U.intFromUnique (U.supplyValue us)) .&. 0xffffff)

-- -------------------------------------------------------------------

instance Pretty BcVar where
  ppr (BcVar v) = ppr v
  ppr (BcReg n) = char 'r' <> int n
  
instance Pretty Label where
  ppr lbl = text (show lbl)

--instance Pretty CompOp where

instance Pretty BinOp where
  ppr OpAdd = char '+'
  ppr OpSub = char '-'
  ppr OpMul = char '*'
  ppr OpDiv = char '/'
  ppr CmpGt = char '>'
  ppr CmpLe = text "<="
  ppr CmpGe = text ">="
  ppr CmpLt = char '<'
  ppr CmpEq = text "=="
  ppr CmpNe = text "/="

instance Pretty b => Pretty (BcIns' b e x) where
  ppr (Label _) = empty
  ppr (Assign r rhs) = ppr r <+> char '=' <+> ppr rhs
  ppr (Eval _ lives r) =
    text "eval" <+> ppr r <+> 
         braces (hsep (commaSep (map ppr (S.toList lives))))
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
       ppr tag <> colon <+> ppr target
         <+> braces (hsep (commaSep (map ppr (S.toList lives))))
  ppr (Call rslt f args) =
    align $
      (case rslt of
         Nothing -> text "return"
         Just (r,_,_) -> ppr r <+> char '=') <+>
      ppr f <> parens (hsep (commaSep (map ppr args)))
  ppr (Ret1 r) = text "return" <+> ppr r
  ppr Update = text "update"
  ppr Stop = text "stop"

instance Pretty BcRhs where
  ppr (Move r) = ppr r
  ppr (Load op) = ppr op
  ppr (BinOp op ty src1 src2) =
    ppr src1 <+> ppr op <+> ppr src2 <+> char '<' <> (ppr ty) <> char '>'
  ppr (Fetch r offs) =
    text "Mem[" <> ppr r <+> char '+' <+> int offs <> char ']'
  ppr (Alloc ctor args) =
    text "alloc(" <> hsep (commaSep (map ppr (ctor:args))) <> char ')'
  ppr (AllocAp args) =
    text "alloc_ap(" <> hsep (commaSep (map ppr args)) <> char ')'

instance Pretty OpTy where
  ppr Int32Ty = text "i32"
  ppr Float32Ty = text "f32"

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

tst1 = do
  pprint ((Assign (BcReg 1) (BinOp OpAdd Int32Ty (BcReg 2) (BcReg 3))) :: BcIns O O)
  pprint ((Assign (BcReg 2) (Fetch (BcReg 2) 42)) :: BcIns O O)

-- -------------------------------------------------------------------

mapLabels :: (l1 -> l2) -> BcIns' l1 e x -> BcIns' l2 e x
mapLabels f ins = case ins of
  Label l      -> Label (f l)
  Assign x rhs -> Assign x rhs
  Eval l lv x     -> Eval (f l) lv x
  Store x n y  -> Store x n y
  Ret1 x       -> Ret1 x
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
  vcat (map ppN middles) $$
  ppMaybeC ppN exit
 where (entry, middles, exit) = blockToNodeList blk

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
insMkAp r args = mkMiddle $ Assign r (AllocAp args)

insMove :: BcVar -> BcVar -> BcGraph O O
insMove dst src = mkMiddle $ Assign dst (Move src)

insAlloc :: BcVar -> BcVar -> [BcVar] -> BcGraph O O
insAlloc r dcon args = mkMiddle $ Assign r (Alloc dcon args)

insStore :: BcVar -> Int -> BcVar -> BcGraph O O
insStore base offs val = mkMiddle $ Store base offs val

insFetch :: BcVar -> BcVar -> Int -> BcGraph O O
insFetch dst base offs = mkMiddle $ Assign dst (Fetch base offs)

insEval :: BlockId -> BcVar -> BcGraph O C
insEval b r = mkLast $ Eval b S.empty r

insRet1 :: BcVar -> BcGraph O C
insRet1 r = mkLast $ Ret1 r

insCase :: CaseType -> BcVar -> [(BcTag, BlockId)] -> BcGraph O C
insCase cty r targets =
  mkLast $ Case cty r (map (\(t, b) -> (t, S.empty, b)) targets)

insGoto :: BlockId -> BcGraph O C
insGoto l = mkLast $ Goto l

insCall :: Maybe (BcVar, BlockId) -> BcVar -> [BcVar] -> BcGraph O C
insCall kont f args = mkLast $ Call kont' f args
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
    , bcoFreeVars  :: Int
    }
  | BcoCon
    { bcoType :: BcoType -- ^ Always 'Con'.  Only for completeness.
    , bcoDataCon :: Id   -- ^ The constructor 'Id'.
    , bcoFields :: [Either BcConst Id]
    }
  | BcConInfo
    { bcoConTag :: Int
    }

type BytecodeObject = BytecodeObject' (Graph BcIns O C)

type BCOs = M.Map Id BytecodeObject
type FinalBCOs = M.Map Id (BytecodeObject' FinalCode)



data BcoType
  = BcoFun Int  -- arity
  | Thunk
  | CAF
  | Con
  deriving Eq

bcoArity :: BytecodeObject' g -> Int
bcoArity BcObject{ bcoType = BcoFun n } = n
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

instance Pretty BcoType where
  ppr (BcoFun n) = text "FUN_" <> int n
  ppr Thunk = text "THUNK"
  ppr CAF = text "CAF"
  ppr Con = text "CON"

instance Pretty g => Pretty (BytecodeObject' g) where
  ppr bco@BcObject{} =
    align $ ppr (bcoType bco) <> char ':' <> int (bcoFreeVars bco) $+$
            text "gbl: " <> align (ppr (bcoGlobalRefs bco)) $+$
            (indent 2 $ ppr (bcoCode bco))
             -- pprGraph ppr (\l -> ppr l <> colon) (bcoCode bco))
  ppr BcoCon{ bcoDataCon = dcon, bcoFields = fields } =
     ppr dcon <+> hsep (map pp_fld fields)
    where pp_fld (Left l) = ppr l
          pp_fld (Right x) = ppr x
  ppr BcConInfo{ bcoConTag = tag } =
    text "CONSTR" <> parens (text "tag:" <> ppr tag)

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
            mkMiddles [Assign (BcReg 1) (BinOp OpAdd Int32Ty (BcReg 1) (BcReg 0)),
                       Assign (BcReg 0) (BinOp OpSub Int32Ty (BcReg 0) (BcReg 2))] <*>
            mkLast (CondBranch CmpGt Int32Ty (BcReg 1) (BcReg 0) l2 l3))
           |*><*|
           (mkFirst (Label l3) <*> mkLast (Ret1 (BcReg 1)))
  (g1, lives1, _) <- analyzeAndRewriteBwd livenessAnalysis2 (JustC l)
                       g0 noFacts
  return $
    pprGraph ppr (ppL lives1) g1
 where
  ppL lives l = ppr l <> colon <+> text "--- lives=" <> ppr (fromMaybe S.empty (lookupFact l lives))

tst3 :: [BcVar]
tst3 = universeBi $ Assign (BcReg 1) (BinOp OpAdd Int32Ty (BcReg 1) (BcReg 0))

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
  biplate (Alloc rt rs) = plate Alloc |* rt ||* rs
  biplate (AllocAp rs) = plate AllocAp ||* rs
  biplate rhs = plate rhs
