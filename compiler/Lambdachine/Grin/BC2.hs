{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Lambdachine.Grin.BC2 where

--import Lambdachine.Grin.ZipCFG
import Lambdachine.Id
import Lambdachine.Utils.Pretty

import Compiler.Hoopl
import Control.Monad.State
import Data.Maybe ( maybeToList )

type BlockId = Label

data BcIns e x where
  Label  :: Label -> BcIns C O
  -- O/O stuff
  Assign :: BcVar -> BcRhs        -> BcIns O O
  Eval   :: BcVar                 -> BcIns O O
  Store  :: BcVar -> Int -> BcVar -> BcIns O O
  -- O/C stuff
  Goto   :: BlockId                -> BcIns O C
  CondBranch :: CompOp -> OpTy -> BcVar -> BcVar
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

data BcLit
  = BcIntLit Integer
  deriving (Eq, Ord)

data BcLoadOperand
  = LoadLit BcLit
  | LoadGlobal Id
  | LoadClosureVar Int
  | LoadBlackhole
  deriving (Eq, Ord)
  
data CompOp
  = CmpGt | CmpLe | CmpGe | CmpLt | CmpEq | CmpNe
  deriving (Eq, Ord)

data BinOp
  = OpAdd | OpSub | OpMul | OpDiv
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

-- -------------------------------------------------------------------

instance Pretty BcVar where
  ppr (BcVar v) = ppr v
  ppr (BcReg n) = char 'r' <> int n
  
instance Pretty Label where
  ppr lbl = text (show lbl)

instance Pretty CompOp where
  ppr CmpGt = char '>'
  ppr CmpLe = text "<="
  ppr CmpGe = text ">="
  ppr CmpLt = char '<'
  ppr CmpEq = text "=="
  ppr CmpNe = text "/="

instance Pretty BinOp where
  ppr OpAdd = char '+'
  ppr OpSub = char '-'
  ppr OpMul = char '*'
  ppr OpDiv = char '/'

instance Pretty BcLit where
  ppr (BcIntLit n) = ppr n

instance Pretty (BcIns e x) where
  ppr (Label _) = empty
  ppr (Assign r rhs) = ppr r <+> char '=' <+> ppr rhs
  ppr (Eval r) = text "eval" <+> ppr r
  ppr (Store base offs val) =
    text "Mem[" <> ppr base <> char '+' <> int offs <> text "] = " <> ppr val
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
    ppr f <> parens (cat (commaSep (map ppr args)))
  ppr (Ret1 r) = text "return" <+> ppr r

instance Pretty BcRhs where
  ppr (Move r) = ppr r
  ppr (Load op) = ppr op
  ppr (BinOp op ty src1 src2) =
    ppr src1 <+> ppr op <+> ppr src2 <+> char '<' <> (ppr ty) <> char '>'
  ppr (Fetch r offs) =
    text "Mem[" <> ppr r <> char '+' <> int offs <> char ']'
  ppr (Alloc ctor args) =
    text "alloc(" <> cat (commaSep (map ppr (ctor:args))) <> char ')'
--   ppr (AllocAp args) =
--     text "alloc_ap(" <> commaSep (map ppr args) <> char ')'

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

pprGraph :: NodePpr n -> Graph n e x -> PDoc
pprGraph ppN GNil = text "{}"
pprGraph ppN (GUnit blk) =
    char '{' $$ pprBlock ppN blk $$ char '}'
pprGraph ppN (GMany entry blocks exit) =
    char '{' $$
      indent 2 (ppMaybeO (pprBlock ppN) entry $$
                vcat pp_blocks $$
                ppMaybeO (pprBlock ppN) exit) $$
    char '}'
   where
     pp_blocks = map pp_block (mapToList blocks)
     pp_block (l, blk) =
       ppr l <> colon $$ indent 3 (pprBlock ppN blk)

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
-- No fucking idea how to get this to work.
pprBlock' :: forall n e x. -- PDoc ~ IndexedCO x PDoc PDoc =>
             NodePpr n -> Block n e x -> IndexedCO x PDoc PDoc
pprBlock' ppN block =
  foldBlockNodesF f block (empty :: IndexedCO x PDoc PDoc)
 where
   f :: n e1 x1 -> PDoc -> PDoc
   f n d = d $$ ppN n
-}

------------------------------------------------------------------------

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
    
tst2 = pprint $ runBcM $ do
  l <- freshLabel
  let g0 = mkFirst (Label l) <*>
           mkMiddles [Assign (BcReg 1) (Move (BcReg 2)),
                      Store (BcReg 0) 5 (BcReg 1),
                      Assign (BcReg 3) (Move (BcReg 2))
                     ] <*>
           mkLast (Goto l)
  return $
    pprGraph ppr g0


newtype BcGraph e x = BcGraph (Graph BcIns e x)
