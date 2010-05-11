module Lambdachine.Stg where

import Lambdachine.Utils
import Lambdachine.Name

type Var = Name

type Literal = Int
type DataCon = String

data PrimOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

data Atom
  = Var Var
  | Lit Literal
  deriving (Eq, Show)
           
data Obj
  = Fun [Var] [Var] Term -- fvs params body
  | Con DataCon [Atom]
  | Thunk [Var] Term
  deriving (Eq, Show)

data Term
  = Atom Atom
  | App Var [Atom]
  | POp PrimOp [Atom]
  | Case Term Var [Alt]
  | LetRec [(Var, Obj)] Term
  | Pos Int Term
  deriving (Eq, Show)

type Alt = (AltCon, Term)

data AltCon
  = DataAlt DataCon [Var]
  | LitAlt Literal
  | DEFAULT
  deriving (Eq, Show)

viewTerm :: Term -> Term
viewTerm (Pos _ t) = viewTerm t
viewTerm t = t

instance Pretty Atom where
  ppr (Var v) = ppr v
  ppr (Lit l) = ppr l
  
instance Pretty PrimOp where
  ppr Add = text "(+)"
  ppr Sub = text "(-)"
  ppr Mul = text "(*)"
  ppr Div = text "(/)"

instance Pretty Term where
  ppr (Atom a) = ppr a
  ppr (App v args) = parens $ ppr v <+> align (hsep (map ppr args))
  ppr (POp op args) = parens $ ppr op <+> sep (map ppr args)
  ppr (Case e v [(alt, e')]) =
    keyword "case" <+> ppr e <+> keyword "of" <+> ppr v <+> char '{'
      <+> ppr alt <+> text "->" <> linebreak <> ppr e' <> char '}'
  ppr (Case e v alts) =
    parens $
    hang 2 (keyword "case" <+> ppr e <+> keyword "of" <+> ppr v <+> char '{' </>
            vcat (map ppr_alt alts)) <> char '}'
  ppr (LetRec binds body) =
    parens $ align $
    hang 2 (keyword "let" </> align (cat (map ppr_bind binds))) </>
    keyword "in" </> ppr body
  ppr (Pos n t) =
    int n <> char ':' <> ppr t
    
instance Pretty Obj where
  ppr (Fun _ params body) =
    text "FUN" <> parens (sep (map ppr params) <+> text "->" </>
                          ppr body)
  ppr (Con dcon args) =
    text "CON" <> parens (text dcon <+> sep (map ppr args))
  ppr (Thunk _ t) =
    text "THUNK" <> parens (ppr t)

instance Pretty AltCon where
  ppr (DataAlt dcon vs) = text dcon <+> sep (map ppr vs)
  ppr (LitAlt lit) = ppr lit
  ppr DEFAULT = char '_'
  
ppr_alt :: Alt -> PDoc
ppr_alt (alt, term) =
  ppr alt <+> text "->" </> hang 2 (ppr term)
  
ppr_bind :: (Var, Obj) -> PDoc
ppr_bind (v, obj) =
  hang 2 $ ppr v <+> char '=' </> ppr obj

lit :: Literal -> Term
lit = Atom . Lit

var :: Name -> Term
var = Atom . Var
