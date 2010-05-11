{-# LANGUAGE BangPatterns, PatternGuards #-}
module Lambdachine.Core where

import Lambdachine.Utils
import qualified Lambdachine.Type as Ty
import Lambdachine.Type ( Type )

newtype Id = Id String
data DataCon = DataCon String Int

data Literal
  = IntLit Int
  deriving (Eq, Show)

data Expr b
  = Var Id
  | Lit Literal
  | App (Expr b) (Arg b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | Case (Expr b) b Type [Alt b]
  | Type Type

type Arg b = Expr b

type Alt b = (AltCon, [b], Expr b)

data AltCon
  = DataAlt DataCon
  | LitAlt Literal
  | DEFAULT

data Bind b
  = NonRec b (Expr b)
  | Rec [(b, Expr b)]


----------------------------------------------------------------------
-- ** Utilities

-- | N-ary application.
app :: Expr b   -- ^ The function.
    -> [Expr b] -- ^ Its arguments.
    -> Expr b   -- ^ Nested unary application.
app fun [] = fun
app fun (arg:args) = app (App fun arg) args

-- | N-ary lambda abstraction.
lam :: [b]    -- ^ Formal parameters.
    -> Expr b -- ^ Function body.
    -> Expr b -- ^ Nested lambdas.
lam [] body = body
lam (p:ps) body = Lam p (lam ps body)

let1 :: b -> Expr b -> Expr b -> Expr b
let1 var expr body =
  Let (NonRec var expr) body

let_ :: [(b, Expr b)] -> Expr b -> Expr b
let_ binds body = foldr (uncurry let1) body binds

letrec :: [(b, Expr b)] -> Expr b -> Expr b
letrec [] body = body
letrec [(var, expr)] body = Let (NonRec var expr) body
letrec binds body = Let (Rec binds) body

-- | View nested unary application as n-ary application.  Inverse of
-- @app@.
--
-- If the input is of the form @(..((f e1) e2) ... eN)@ returns @(f,
-- [e1,e2,..,eN])@.  If the input is not an application the returned
-- list will be empty.
viewApp :: Expr b -> (Expr b, [Expr b])
viewApp (App f_ arg_) = go f_ [arg_]
 where
   go (App f arg) !args = go f (arg:args)
   go f args = (f, args)
viewApp e = (e, [])

-- | View unary abstraction as n-ary abstraction.  Inverse of @lam@.
--
-- Multi-argument lambdas are represented internally as nested
-- one-argument lambdas.  This reverses this view.
viewLam :: Expr b -> ([b], Expr b)
viewLam expr_ = go [] expr_
 where
   go binders (Lam binder expr) = go (binder:binders) expr
   go binders expr              = (reverse binders, expr)
{-
class LiftExpr e where
  toExpr :: e -> Expr Id

instance LiftExpr Id where toExpr = Var
instance LiftExpr Literal where toExpr = Lit
instance LiftExpr (Expr Id) where toExpr = id
-}
----------------------------------------------------------------------
-- ** Pretty Printing

instance Pretty Id where
  ppr (Id n) = text n

instance Pretty DataCon where
  ppr (DataCon n _) = text n

instance Pretty Literal where
  ppr (IntLit i) = int i

instance Pretty b => Pretty (Expr b) where
  ppr = ppr_expr id

ppr_expr :: Pretty b => (PDoc -> PDoc) -> Expr b -> PDoc
ppr_expr _ (Var name) = ppr name
ppr_expr _ (Lit lit) = ppr lit
ppr_expr _ (Type t) = colour1 (text "ty")
ppr_expr add_par expr@App{} =
  case viewApp expr of
    (fun, args) ->
      add_par $ hang 2 (ppr fun </> sep (map (ppr_expr parens) args))
ppr_expr add_par expr@Lam{} =
  let (binders, body) = viewLam expr in
  add_par $ hang 2 (char '\\' <+> sep (map ppr binders) <+> arrow </>
                    ppr_expr id body)
ppr_expr add_par (Case expr var ty alts) =
  add_par $
  sep [sep [keyword "case" <+> ppr_expr id expr,
            keyword "of" <+> ppr var <+> char '{'],
       nest 2 (sep (punctuate semi [])),
       char '}']
ppr_expr add_par (Let (Rec []) expr) = ppr_expr add_par expr
ppr_expr add_par (Let bind expr) =
  add_par $
  sep [sep [keyword "let" <+> ppr_bind bind, keyword "in"]
      ,nest 2 (ppr expr)]
  
-- PRE: binding non-empty
ppr_bind :: Pretty b => Bind b -> PDoc
ppr_bind bind =
  case bind of
    NonRec var expr ->
      align $ ppr_one_bind (var, expr)
    Rec binds ->
      keyword "rec" <$> indent 2 
      (let (b:bs) = map ppr_one_bind binds in
        sep ((char '{' <+> b) : map (char ';' <+>) bs)) <+> char '}'
 where
   ppr_one_bind (var, expr) =
     hang 2 (ppr var <+> char '=' </> ppr expr)
  

tst1 :: Expr Id
tst1 = let x = Id "x"; y = Id "y"; z = Id "z" in
       lam [x,y] $
         app (Var z) [Lit (IntLit 42), app (Var x) [Lit (IntLit 23)]]

tst2 :: Expr Id
tst2 = let x = Id "x"; y = Id "y"; z = Id "z" in
       letrec [(x ,Var (Id "unotahunaoheusnaheuntahuosetnaosehuonsahuo")) 
              ,(y, Lit (IntLit 42))]
         (Var z)
