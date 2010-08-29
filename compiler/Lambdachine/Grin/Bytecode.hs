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
import qualified Lambdachine.Utils.Unique as U

import Compiler.Hoopl
import Control.Monad.State
import Data.Maybe ( maybeToList, fromMaybe )
--import qualified Data.Set as S
import Data.Generics.Uniplate.Direct
import Data.Bits ( (.&.) )

type BlockId = Label

data BcIns e x where
  Label  :: Label -> BcIns C O
  -- O/O stuff
  Assign :: BcVar -> BcRhs        -> BcIns O O
  Eval   :: BcVar                 -> BcIns O O
  Store  :: BcVar -> Int -> BcVar -> BcIns O O
  -- O/C stuff
  Goto   :: BlockId                -> BcIns O C
  CondBranch :: BinOp -> OpTy -> BcVar -> BcVar
             -> BlockId -> BlockId -> BcIns O C
  Case :: CaseType -> BcVar 
       -> [(BcTag, BlockId)]       -> BcIns O C
  Call :: Maybe (BcVar, BlockId)
       -> BcVar -> [BcVar]         -> BcIns O C
  Ret1 :: BcVar                    -> BcIns O C

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
  deriving (Eq, Ord)

data OpTy = Int32Ty | Float32Ty
  deriving (Eq, Ord)

data BcVar = BcVar !Id
           | BcReg {-# UNPACK #-} !Int
  deriving (Eq, Ord)



instance NonLocal BcIns where
  entryLabel (Label l) = l
  successors (Goto l) = [l]
  successors (CondBranch _ _ _ _ tl fl) = [fl, tl]
  successors (Case _ _ targets) = map snd targets
  successors (Call mb_l _ _) = maybeToList (snd `fmap` mb_l)
  successors (Ret1 _) = []

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

instance Pretty (BcIns e x) where
  ppr (Label _) = empty
  ppr (Assign r rhs) = ppr r <+> char '=' <+> ppr rhs
  ppr (Eval r) = text "eval" <+> ppr r
  ppr (Store base offs val) =
    text "Mem[" <> ppr base <+> char '+' <+> int offs <> text "] = " <> ppr val
  ppr (Goto bid) = text "goto" <+> ppr bid
  ppr (CondBranch cmp ty r1 r2 true false) =
    text "if" <+> ppr r1 <+> ppr cmp <+> ppr r2 <+>
      char '<' <> ppr ty <> char '>' <+> text "then goto" <+> ppr true <+>
      brackets (text "else goto" <+> ppr false)
  ppr (Case _ r targets) =
    text "case" <+> ppr r $$ indent 2 (vcat (map ppr_target targets))
   where ppr_target (tag, target) = ppr tag <> colon <+> ppr target
  ppr (Call rslt f args) =
    (case rslt of
       Nothing -> text "return"
       Just (r,_) -> ppr r <+> char '=') <+>
    ppr f <> parens (hsep (commaSep (map ppr args)))
  ppr (Ret1 r) = text "return" <+> ppr r

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
  ppr (LoadGlobal x) = ppr x
  ppr (LoadClosureVar n) = text "Node[" <> int n <> char ']'
  ppr LoadBlackhole = text "<blackhole>"

instance Pretty BcTag where
  ppr DefaultTag = text "_"
  ppr (Tag n) = int n
  ppr (LitT n) = char '#' <> text (show n)

tst1 = do
  pprint (Assign (BcReg 1) (BinOp OpAdd Int32Ty (BcReg 2) (BcReg 3)))
  pprint (Assign (BcReg 2) (Fetch (BcReg 2) 42))

-- -------------------------------------------------------------------

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

insEval :: BcVar -> BcGraph O O
insEval r = mkMiddle $ Eval r

insRet1 :: BcVar -> BcGraph O C
insRet1 r = mkLast $ Ret1 r

insCase :: CaseType -> BcVar -> [(BcTag, BlockId)] -> BcGraph O C
insCase cty r targets = mkLast $ Case cty r targets

insGoto :: BlockId -> BcGraph O C
insGoto l = mkLast $ Goto l

insCall :: Maybe (BcVar, BlockId) -> BcVar -> [BcVar] -> BcGraph O C
insCall kont f args = mkLast $ Call kont f args

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

type BytecodeObject = BytecodeObject' (Graph BcIns O C)

data BcoType
  = BcoFun Int  -- arity
  | Thunk
  | CAF
  | Con

data BcConst
  = CInt Integer
  | CStr String
  | CRef Label
  deriving (Eq, Ord, Show)

instance Pretty BcConst where
  ppr (CInt n) = ppr n
  ppr (CRef l) = ppr l
  ppr (CStr s) = text (show s)

instance Pretty BcoType where
  ppr (BcoFun n) = text "FUN_" <> int n
  ppr Thunk = text "THUNK"
  ppr CAF = text "CAF"
  ppr Con = text "CON"

instance Pretty BytecodeObject where
  ppr bco@BcObject{} =
    align $ text "BCO " <> ppr (bcoType bco) <> char ':' <> int (bcoFreeVars bco) $+$
            text "gbl: " <> align (ppr (bcoGlobalRefs bco)) $+$
            (indent 2 $ pprGraph ppr (\l -> ppr l <> colon) (bcoCode bco))
  ppr BcoCon{ bcoDataCon = dcon, bcoFields = fields } =
     ppr dcon <+> hsep (map pp_fld fields)
    where pp_fld (Left l) = ppr l
          pp_fld (Right x) = ppr x



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

instance Biplate (BcIns e x) BcVar where
  biplate (Assign r rhs) = plate Assign |* r |+ rhs
  biplate (Eval r) = plate Eval |* r
  biplate (Store r n r') = plate Store |* r |- n |* r'
  biplate (CondBranch c t r1 r2 l1 l2) =
    plate (CondBranch c t) |* r1 |* r2 |- l1 |- l2
  biplate (Case ty r targets) =
    plate (Case ty) |* r |- targets
  biplate (Call mb_r f args) =
    plate Call |+ mb_r |* f ||* args
  biplate (Ret1 r) = plate Ret1 |* r
  biplate l = plate l

instance Biplate BcRhs BcVar where
  biplate (Move r) = plate Move |* r
  biplate (BinOp op ty r1 r2) = plate (BinOp op ty) |* r1 |* r2
  biplate (Fetch r n) = plate Fetch |* r |- n
  biplate (Alloc rt rs) = plate Alloc |* rt ||* rs
  biplate (AllocAp rs) = plate AllocAp ||* rs
  biplate rhs = plate rhs

instance Biplate (Maybe (BcVar, Label)) BcVar where
  biplate Nothing = plate Nothing
  biplate (Just (x,y)) = plate (\x' -> Just (x',y)) |* x


