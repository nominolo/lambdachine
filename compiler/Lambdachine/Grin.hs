module Lambdachine.Grin where

import Lambdachine.Utils
import Lambdachine.Id

-- These should become proper data types
type Var = Id
type Literal = Integer
type DataCon = Var

data Module = Module
  { moduleId :: ModuleId
  , moduleFuns :: [FunDef]
  , moduleCafs :: [(Var, Expr)]
  } deriving (Show, Eq)

data ModuleId = ModuleId
  { modulePackage :: String
  , moduleName :: String
  } deriving (Show, Eq)

data FunDef = FunDef
  { funDefName :: Var
  , funDefArgs :: [Var]
  , funDefBody :: Expr
  } deriving (Show, Eq)

data Expr
  = Expr :>>= Lambda
  | Expr :>> Expr
  | App Var [Var]
  | Prim Var [Var]
  | Case Var [Alt]
  | Store Value
  | Update Int Var Var  -- size, ptr, val
  | Unit Value
  | Eval UpdFlag Var
  deriving (Show, Eq)

data UpdFlag
  = AlwaysUpdate
  | NeverUpdate
  deriving (Show, Eq)

data Alt = Value :> Expr
  deriving (Show, Eq)

data Lambda = Value :-> Expr
  deriving (Show, Eq)

infix 1 :>
infix 2 :->
infixr 3 :>>=

data Value
  = Lit Literal
  | Node Var [Var]
  | Var Var
  | Void
  | Hole Int
    -- ^ Used to initialise mutually recursive nodes.
  deriving (Show, Eq)

instance Pretty Expr where
  ppr (e :>>= (p :-> e')) =
    ppr e <> text "; \\" <> ppr p <+> text "->" <> linebreak <> ppr e'
  ppr (e :>> e') =
    ppr e <> char ';' <> linebreak <> ppr e'
  ppr (Prim op args) =
    ppr op <+> hsep (map ppr args)
  ppr (App f args) =
    ppr f <+> hsep (map ppr args)
  ppr (Case x alts) =
    keyword "case" <+> ppr x <+> keyword "of" <> linebreak <>
    indent 2 (vcat (map ppr alts))
  ppr (Store val) =
    keyword "store" <+> ppr val
  ppr (Update n p val) =
    keyword "update" <+> ppr p <+> ppr val
  ppr (Unit val) =
    keyword "unit" <+> ppr val
  ppr (Eval u x) =
    keyword "eval" <> brackets (ppr u) <+> ppr x

instance Pretty UpdFlag where
  ppr AlwaysUpdate = char 'u'
  ppr NeverUpdate = char 'n'

instance Pretty Alt where
  ppr (pat :> e) =
    hang 2 $ ppr pat <+> text "->" <> linebreak <> ppr e

instance Pretty Value where
  ppr (Lit n) = text (show n)
  ppr (Node x xs) =
    parens $ hsep (map ppr (x:xs))
  ppr (Var x) = ppr x
  ppr Void = text "()"
  ppr (Hole n) = text "hole" <> brackets (int n)

instance Pretty ModuleId where
  ppr m = text (moduleName m)

instance Pretty Module where
  ppr m =
    keyword "module" <+> ppr (moduleId m) <> linebreak <>
    vcat (map ppr (moduleFuns m)) <> linebreak <>
    vcat (map (\(f, e) -> ppr f <+> char '=' <> linebreak <>
                          indent 2 (ppr e))
              (moduleCafs m))
instance Pretty FunDef where
  ppr (FunDef f args body) =
    ppr f <+> hsep (map ppr args) <+> char '=' <> linebreak <>
    indent 2 (ppr body)

{-
tst1 =
  Eval AlwaysUpdate m :>>= (Node cInt [m'] :-> 
  Eval AlwaysUpdate n :>>= (Node cInt [n'] :-> 
  Prim intGT [m', n'] :>>= (Var b' :->
  Case b' 
    [Var cTrue :> Unit (Node cNil [])
    ,Var cFalse :>
      (Unit (Lit 1) :>>= (Var o' :->
       Prim intAdd [m', o'] :>>= (Var x' :->
       Store (Node cInt [x']) :>>= (Var p1 :->
       Store (Node fupto [p1, n]) :>>= (Var p2 :->
       Unit (Node cCons [m, p2]))))))])))
 where
   [cInt, cNil, cCons, cTrue, cFalse] = 
     map name ["CI#", "C[]", "C:", "CTrue", "CFalse"]
   [fupto] = map name ["Fupto"]
   [intGT, intAdd] = map name [">#", "+#"]
   [m, m', n, n', b', o', x', p1, p2] = 
     map name ["m", "m'", "n", "n'", "b'", "o'", "x'", "p1", "p2"]
-}