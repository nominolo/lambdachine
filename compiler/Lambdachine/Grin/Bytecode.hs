{-# LANGUAGE ViewPatterns, PatternGuards,
             GeneralizedNewtypeDeriving, TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
module Lambdachine.Grin.Bytecode where

import Prelude hiding ( foldr )

import Lambdachine.Utils
import Lambdachine.Id
import qualified Lambdachine.Grin as Grin
import Lambdachine.Grin ( Value(..) ) --, Expr((:>>=), (:>>), (:->), Unit) )

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State.Strict
import Data.Foldable
import Data.Monoid

import Debug.Trace

data BytecodeObject r c 
  = BcObject
    { bcoType :: BcoType
    , bcoCode :: [BcInstr r c]
    , bcoGlobalRefs :: [Id]
    , bcoConstants :: [c]
    , bcoFreeVars  :: Int
    }   
  | BcoCon
    { bcoType :: BcoType -- ^ Always 'Con'.  Only for completeness.
    , bcoDataCon :: Id   -- ^ The constructor 'Id'.
    , bcoFields :: [Either BcConst Id]
    }

data BcoType
  = BcoFun Int  -- arity
  | Thunk
  | CAF
  | Con

instance Pretty BcoType where
  ppr (BcoFun n) = text "FUN_" <> int n
  ppr Thunk = text "THUNK"
  ppr CAF = text "CAF"
  ppr Con = text "CON"

instance (Pretty r, Pretty c) => Pretty (BytecodeObject r c) where
  ppr bco@BcObject{} =
    align $ text "BCO " <> ppr (bcoType bco) <> char ':' <> int (bcoFreeVars bco) $+$
            text "gbl: " <> align (ppr (bcoGlobalRefs bco)) $+$
            (indent 2 $ ppr (bcoCode bco))
  ppr BcoCon{ bcoDataCon = dcon, bcoFields = fields } =
     ppr dcon <+> hsep (map pp_fld fields)
    where pp_fld (Left l) = ppr l
          pp_fld (Right x) = ppr x

data Label
  = NamedLabel Name
  | InternalLabel !Int
  deriving (Eq, Ord, Show)

instance Pretty Label where
  ppr (NamedLabel n) = ppr n
  ppr (InternalLabel n) = char '.' <> ppr n

newtype BcBlock r c = BcBlock [BcInstr r c]
  deriving (Eq, Ord, Show)

-- | A bytecode instruction.
data BcInstr r c
  = Move r r
  | LoadK r c   -- ^ Load a literal
  | LoadG r Id  -- ^ Load a global function / CAF
  | LoadC r Id  -- ^ Load a constructor descriptor
  | LoadF r Int -- must be a lazy 'Int'
  | LoadBlackhole r
  | BinR BinOp OpTy r r r
  | BinC BinOp OpTy r r c
  | Eval r r
  | Case r [(BcTag, [BcInstr r c])]
  | Ret1 r
  | Fetch r r Int
  | Update r Int r  -- base[n] = r
  | Store r r [r]
    -- ^ @ALLOC rslt, ctor, val1, .., valN@
  | MkAp r r [r]
    -- ^ @MKAP rslt, f, r1, .., rN == ALLOC rslt, AP, f, r1, .., rN@
  | Call (Maybe r) r [r]
    -- ^ @Call Nothing f args@ means tailcall.
    -- 
    -- @Call (Just rslt) f args@ means regular call with result in @rslt@.
  | Nop
  deriving (Eq, Ord, Show)

-- | A bytecode variable.
data BcVar = BcVar !Id
           | BcReg Int
  deriving (Eq, Ord)

instance Pretty BcVar where
  ppr (BcVar x) = ppr x
  ppr (BcReg n) = char 'R' <> ppr n

data BcTag
  = DefaultTag
  | Tag Int
  | LitT Integer
  deriving (Eq, Ord, Show)

instance (Pretty r, Pretty c) => Pretty (BcInstr r c) where
  ppr instr = case instr of
    Move tgt src ->
      text "MOVE    " <> hsep (commaSep [ppr tgt, ppr src])
    LoadK r c ->
      text "LOADLIT " <> hsep (commaSep [ppr r, ppr c])
    LoadG r g ->
      text "LOADGBL " <> hsep (commaSep [ppr r, ppr g])
    LoadF r n ->
      text "LOADENV " <> hsep (commaSep [ppr r, int n])
    LoadBlackhole r ->
      text "LOADBLK " <> ppr r
    Eval t r ->
      text "EVAL    " <> hsep (commaSep [ppr t, ppr r])
    Ret1 r ->
      text "RET1    " <> ppr r
    Fetch dst src offs ->
      text "LOADFLD " <> hsep (commaSep [ppr dst, ppr src, ppr offs])
    Store dst tag args ->
      text "ALLOC   " <> hsep (commaSep (map ppr (dst:tag:args)))
    Update base offs arg ->
      text "STORE   " <> hsep (commaSep [ppr base, ppr offs, ppr arg])
    MkAp dst f args ->
      text "ALLOCAP " <> hsep (commaSep (map ppr (dst:f:args)))
    Call Nothing f args ->
      text "CALLT   " <> hsep (commaSep (map ppr (f:args)))
    Call (Just r) f args ->
      text "CALL    " <> hsep (commaSep (map ppr (r:f:args)))
    Case r alts ->
      vcat [ text "CASE    " <> ppr r
           , indent 1 $ vcat $ map pp_alt alts
           ]
     where pp_alt (n, is) = ppr n <> char ':' <+> indent 1 (vcat (map ppr is))
    Nop ->
      text "NOP"
    BinR op ty r1 r2 r3 ->
      ppr op <> ppr ty <+> hsep (commaSep [ppr r1, ppr r2, ppr r3])

instance (Pretty r, Pretty c) => Pretty (BcBlock r c) where
  ppr (BcBlock instrs) = align $ vcat (map ppr instrs)
instance Pretty BcTag where
  ppr DefaultTag = text "_"
  ppr (Tag n) = int n
  ppr (LitT n) = char '#' <> text (show n)

data OpTy = Int32Ty | Float32Ty
  deriving (Eq, Ord, Show)

data BinOp
  = PrAdd | PrSub | PrMul | PrDiv
  | PrGt | PrLe | PrGe | PrLt | PrEq | PrNe
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  ppr PrAdd = text "ADD"
  ppr PrSub = text "SUB"
  ppr PrMul = text "MUL"
  ppr PrDiv = text "DIV"
  ppr PrGt = text "ISGT"
  ppr PrLe = text "ISLE"
  ppr PrGe = text "ISGE"
  ppr PrLt = text "ISLT"
  ppr PrEq = text "ISEQ"
  ppr PrNe = text "ISNE"

instance Pretty OpTy where
  ppr Int32Ty = char 'I'
  ppr Float32Ty = char 'D'

isComparison :: BinOp -> Bool
isComparison op = op >= PrGt && op <= PrNe

-- | Can we swap the operands of this operation?
isCommutative :: BinOp -> Bool
isCommutative PrAdd = True
isCommutative PrMul = True
isCommutative _ = False

data BcConst
  = CInt Integer
  | CStr String
  | CRef Label
  deriving (Eq, Ord, Show)

instance Pretty BcConst where
  ppr (CInt n) = ppr n
  ppr (CRef l) = ppr l
  ppr (CStr s) = text (show s)

data Bag a
  = Empty
  | One a
  | List [a]
  | Cat (Bag a) (Bag a)
  deriving Show

instance Foldable Bag where
  foldr _ z Empty = z
  foldr f z (One a) = f a z
  foldr f z (List l) = foldr f z l
  foldr f z (Cat l r) = foldr f (foldr f z r) l

instance Eq a => Eq (Bag a) where
  l == r = toList l == toList r

instance Monoid (Bag a) where
  mempty = emptyBag
  l `mappend` r = catBags l r

emptyBag :: Bag a
emptyBag = Empty

catBags :: Bag a -> Bag a -> Bag a
catBags Empty xs = xs
catBags xs Empty = xs
catBags xs ys    = Cat xs ys

listToBag :: [a] -> Bag a
listToBag [] = Empty
listToBag [x] = One x
listToBag xs = List xs

singletonBag :: a -> Bag a
singletonBag = One

-- | Add element at the end of the bag.
snocBag :: Bag a -> a -> Bag a
snocBag bag a = catBags bag (One a)

newtype BcInstrs r c = Instrs (Bag (BcInstr r c))
  deriving (Eq, Show, Monoid)

instrToList :: BcInstrs r c -> [BcInstr r c]
instrToList (Instrs b) = toList b

{-
type Instr1 = BcInstr Name BcConst
type Bytecode1 = BcBlock Name BcConst

newtype BcGen a = BcGen { unBcGen :: State BcGenState a }
  deriving (Functor, Applicative, Monad, MonadState BcGenState)

data BcOp
  = BcConst Integer
  | BcVar Grin.Var
  | BcFetch Int BcOp

data BcGenState = BcGenState
  { bcUniques   :: !(Supply Unique)
  , bcAliases   :: M.Map Grin.Var BcOp
  , bcInstrs    :: [BcInstr Name BcConst]
  , bcCurrLabel :: Label
  , bcBlocks    :: M.Map Label Bytecode1 }

type BytecodeModule = M.Map Label Bytecode1

runBcGen :: Supply Unique -> BcGen a -> a
runBcGen uniques (BcGen m) = evalState m s0
 where
   s0 = BcGenState uniques M.empty [] (error "No current label") M.empty

namedLabel :: Name -> BcGen ()
namedLabel nm = do
  finishCurrentLabel (NamedLabel nm)
  
-- | Add accumulated instructions with the current label and
-- set the new current label.
finishCurrentLabel :: Label -> BcGen ()
finishCurrentLabel l = do
  modify' $ \st ->
    let instrs = bcInstrs st in
    st{ bcBlocks = 
          if not (null instrs) then
            M.insert (bcCurrLabel st) (BcBlock $ reverse instrs)
                     (bcBlocks st)
           else bcBlocks st
        , bcInstrs = []
        , bcCurrLabel = l }

toBytecode :: Supply Unique -> Grin.Module -> BytecodeModule
toBytecode uniques mdl =
  runBcGen uniques $ bc_module mdl

bc_module :: Grin.Module -> BcGen BytecodeModule
bc_module mdl = do
  forM_ (Grin.moduleFuns mdl) $ \fundef -> trace (pretty (Grin.funDefName fundef)) $ do
    namedLabel (idName (Grin.funDefName fundef))
    modify' $ \st -> 
      st{ bcAliases = M.fromList [(v, BcVar v) 
                                  | v <- Grin.funDefArgs fundef ] }
    bc_expr (Grin.funDefBody fundef)
  gets bcBlocks

emit :: Instr1 -> BcGen ()
emit i = modify' $ \st -> st{ bcInstrs = i : bcInstrs st }

alias :: Grin.Var -> BcOp -> BcGen ()
alias var op =
  modify' $ \st -> st{ bcAliases = M.insert var op (bcAliases st) }

getAlias :: Grin.Var -> BcGen BcOp
getAlias var =
  expectJust "getAlias" . M.lookup var <$> gets bcAliases

freshVar :: BcGen Name
freshVar = do
  st <- get
  case split2 (bcUniques st) of
    (us, us') -> do
       put $! st{ bcUniques = us' }
       return $ freshName us "bc"

materialiseVar :: Grin.Var -> BcGen Name
materialiseVar x = do
  aliases <- gets bcAliases
  case M.lookup x aliases of
    Nothing -> error $ "BcGen: Unbound variable \"" ++ pretty x ++ "\""
    Just (BcVar x')
      | x == x' -> return (idName x)
      | otherwise -> materialiseVar x'
    Just (BcConst c) -> do
      emit (LoadK (idName x) (CInt c))
      alias x (BcVar x)
      return (idName x)

bc_expr :: Grin.Expr -> BcGen ()
bc_expr expr_ = case expr_ of
  Grin.Unit (Grin.Lit n) Grin.:>>= (Var x Grin.:-> e1) -> trace "1"$ do
    alias x (BcConst n)
    bc_expr e1
  Grin.Unit (Lit n) -> trace "2"$do
    v <- freshVar
    emit (LoadK v (CInt n))
    emit (Ret1 v)
  Grin.Unit (Var x) -> trace "3"$do
    emit . Ret1 =<< materialiseVar x
  Grin.Unit (Var x) Grin.:>>= (Var x' Grin.:-> e) -> trace "4"$do
    alias x' (BcVar x)
  Grin.Unit (Var x) Grin.:>>= (Node c xs Grin.:-> e) -> trace "5"$do
    x' <- getAlias x
    sequence_ [ alias y (BcFetch n x') | (n,y) <- zip [0..] (c:xs) ]
  Grin.Eval upd x Grin.:>>= (Var x' Grin.:-> e) -> trace "6"$do
    alias x' (BcVar x)
    tag <- freshVar
    emit (Eval tag (idName x))
    bc_expr e
  _ ->
    emit Nop
--  Prim 
-}
