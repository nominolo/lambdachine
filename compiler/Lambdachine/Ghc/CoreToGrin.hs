{-# LANGUAGE ViewPatterns, PatternGuards,
             GeneralizedNewtypeDeriving #-}
-- | Translate GHC Core into (high-level) GRIN.
--
-- TODO: 
-- 
--   - Lambda-lift all local bindings
--
--   - Implement 'tidyGrin'
--   
--   - Keep type information on GRIN ids in order to avoid evaluating
--     primitive types.
--
module Lambdachine.Ghc.CoreToGrin 
  ( toGrinModule )
where

import Lambdachine.Utils
import qualified Lambdachine.Name as N
import Lambdachine.Grin as Grin

import qualified Var as Ghc
import qualified HscTypes as Ghc ( CoreModule(..) )
import qualified Module as Ghc
import qualified Literal as Ghc
import qualified Name as Ghc
import qualified IdInfo as Ghc
import qualified DataCon as Ghc
import qualified CoreSyn as Ghc ( Expr(..) )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr,
                 Bind(..), Expr(Lam, Let, Type, Cast, Note),
                 AltCon(..),
                 collectBinders, flattenBinds, collectArgs )
import Var ( isTyVar )

import Control.Applicative
import Control.Monad.State.Strict
import Data.Char ( ord )
import Data.Either ( partitionEithers )

-- | The transformation monad
newtype Trans a = Trans (State (Supply Unique) a)
  deriving (Functor, Applicative, Monad, MonadState (Supply Unique))

runTrans :: Supply Unique -> Trans a -> a
runTrans s (Trans m) = evalState m s

-- | Converts a 'Ghc.CoreModule' to our GRIN IR.
--
-- Performs no optimisations.
--
toGrinModule :: Supply Unique -> Ghc.CoreModule -> Grin.Module
toGrinModule uniques core_mdl =
  Grin.Module
    { moduleId = toGrinModuleId (Ghc.cm_module core_mdl)
    , moduleFuns = funs }
 where
   (cafs, funs) = 
     runTrans uniques $ toGrinFuns $ Ghc.cm_binds core_mdl

-- | Convert GHC's notion of a module ID into our version.
toGrinModuleId :: Ghc.Module -> Grin.ModuleId
toGrinModuleId mdl =
  ModuleId 
    { modulePackage = Ghc.packageIdString (Ghc.modulePackageId mdl)
    , moduleName = Ghc.moduleNameString (Ghc.moduleName mdl)
    }

toVar :: Ghc.Var -> Grin.Var
toVar gvar = N.name $! show gvar

toDataCon :: Ghc.DataCon -> Grin.DataCon
toDataCon dcon = N.name $! Ghc.getOccString dcon

freshVar :: Show a => a -> Trans Grin.Var
freshVar var = do
  s <- get
  case split2 s of
    (u, s') -> put s' >> return (N.freshName u $! show var)

type Caf = ()

toGrinFuns :: [CoreBind] -> Trans ([Caf], [FunDef])
toGrinFuns binds =
  partitionEithers <$> mapM toGrinFunOrCaf flat_binds
 where
  flat_binds = flattenBinds binds

toGrinFunOrCaf :: (CoreBndr, CoreExpr) -> Trans (Either Caf FunDef)
toGrinFunOrCaf (toVar -> f, collectBinders -> (args_, body)) =
  let args = filter (not . isTyVar) args_ in
  case args of
    [] -> Left <$> toGrinCaf f body
    _ -> Right <$> toGrinFun f (map toVar args) body

toGrinCaf :: Var -> CoreExpr -> Trans Caf
toGrinCaf _ _ = return ()

toGrinFun :: Var -> [Var] -> CoreExpr -> Trans FunDef
toGrinFun f args body =
  FunDef f args <$> toGrinExpr body

-- | Remove all type abstractions, type applications and coercions.
--
-- Input expression must not evaluate to a type.
--
-- Removes notes as well.
eraseTypes :: CoreExpr -> CoreExpr
eraseTypes = go
 where
   go e@(Ghc.Var i) = e
   go e@(Ghc.Lit l) = e
   go (Ghc.App e1 e2)
     | Type _ <- e2 = go e1
     | otherwise = Ghc.App (go e1) (go e2)
   go (Lam x e)
     | isTyVar x = go e
     | otherwise = Lam x (go e)
   go (Let (NonRec x e1) e2)
     | Type _ <- e1 = go e2
     | otherwise = Let (NonRec x (go e1)) (go e2)
   go (Let (Rec ps) e2) =
     Let (Rec (map (second go) ps)) (go e2)
   go (Ghc.Case e1 x ty alts) =
     Ghc.Case (go e1) x ty (map go_alt alts)
   go (Cast e _) = go e
   go (Note _ e) = go e
   go (Type _) = error "eraseTypes: But... that's IMPOSSIBLE!"
  
   go_alt (con, vs, e) =
     (con, filter (not . isTyVar) vs, go e)

--liftLambdas :: 

toGrinLiteral :: Ghc.Literal -> Grin.Literal
toGrinLiteral l = case l of
  Ghc.MachChar c -> fromIntegral (ord c)
  Ghc.MachInt n  -> n
                    
-- | This is the meat.
--
-- PRECOND: All local lets have been lifted to the top-level.
-- 
-- The basic idea:
--
--   - We generate code by switching between strict and lazy contexts.
--     By default we generate code for a strict context, that is, code
--     which returns a WHNF.  This is implemented by 'buildWhnf'.
--
--   - Before calling a function we create thunks for each argument
--     (where necessary) using 'buildThunk'.
--
-- The resulting GRIN code is likely to contain a few redundancies, so
-- 'tidyGrin' should be run afterwards.
--
toGrinExpr :: CoreExpr -> Trans Grin.Expr
toGrinExpr e_ = buildWhnf (eraseTypes e_)

-- | Generate code in a strict context
buildWhnf :: CoreExpr -> Trans Grin.Expr
buildWhnf (Ghc.Lit l) =
  return (Unit (Grin.Lit (toGrinLiteral l)))
buildWhnf (Ghc.Var x) 
  | isDataConId x = return $ Unit . Var $ toVar x
  | otherwise =
    return $ Eval AlwaysUpdate (toVar x)
buildWhnf e@(Ghc.App _ _) = do
  let (fun, args) = collectArgs e
  buildThunks args $ \as ->
    case fun of
      Ghc.Var x
        | isPrimOp x
        -> return $ Prim (toVar x) as
        | isDataConId x
        -> return $ Unit (Node (toVar x) as)
        | otherwise
        -> return $ App (toVar x) as
      _ -> do
        fun' <- buildWhnf fun
        f <- freshVar (text "f")
        return (fun' :>>= (Var f :-> App f as))
buildWhnf (Let (NonRec v e1) e2) = do
  v' <- freshVar v
  e1' <- buildThunk e1
  e2' <- buildWhnf e2
  return (e1' :>>= (Var v' :-> e2'))
buildWhnf (Ghc.Case e1 v _ty alts) = do
  let v' = toVar v
  e1' <- buildWhnf e1
  alts' <- forM alts $ \(con, xs, e2) -> do
    (:>) <$> pat_to_val v' con xs
         <*> buildWhnf e2
  case alts' of
    [(pat :> e2')] ->
      -- case with only one alternative = strict let
      return (e1' :>>= (pat :-> e2'))
    _ ->
      return $ e1' :>>= (Var v' :-> Case v' alts')
buildWhnf _ = return $ Unit Void

-- | Build a thunk corresponding to the input expression.
buildThunk :: CoreExpr -> Trans Grin.Expr
buildThunk (Ghc.Var x) = return (Unit (Var (toVar x)))
buildThunk e@(Ghc.Lit _) = buildWhnf e
buildThunk e@(Ghc.App _ _) = do
  let (fun, args) = collectArgs e
  case fun of
    Ghc.Var x
      | isPrimOp x -> buildWhnf e
      | isDataConId x ->
        buildThunks args $ \as ->
          return $ Store $ Node (toVar x) as
    _ ->
      buildThunks args $ \as -> do
        fun' <- buildWhnf fun
        f <- freshVar (text "f")
        return $ fun' :>>= (Var f :-> Store (Node f as))

buildThunk _ = return $ Unit Void

pat_to_val :: Var -> AltCon -> [CoreBndr]
           -> Trans Value
pat_to_val x DEFAULT [] = return $ Var x
pat_to_val x (DataAlt dcon) xs =
  return (Node (toDataCon dcon) (map toVar xs))
pat_to_val x (LitAlt l) [] =
  return (Lit (toGrinLiteral l))

buildThunks :: [CoreExpr] -> ([Var] -> Trans Grin.Expr) 
            -> Trans Grin.Expr
buildThunks es_ kont = go es_ []
 where
   go [] xs = kont (reverse xs)
   go ((Ghc.Var x):es) xs =
     go es (toVar x : xs)
   go (e:es) xs = do
     e' <- buildThunk e
     x <- freshVar (text "t")
     es' <- go es (x:xs)
     return $ e' :>>= (Var x :-> es')

-- -------------------------------------------------------------------

-- -------------------------------------------------------------------

isDataConId :: Ghc.Id -> Bool
isDataConId x = case Ghc.idDetails x of
  Ghc.DataConWorkId _ -> True
  Ghc.DataConWrapId _ -> True
  _ -> False

isPrimOp :: Ghc.Id -> Bool
isPrimOp x = case Ghc.idDetails x of
  Ghc.PrimOpId _ -> True
  _ -> False

-- -------------------------------------------------------------------

tidyGrin :: Grin.Module -> Grin.Module
tidyGrin m = m

-- file:///Users/ts319/local/share/doc/ghc/html/libraries/ghc-6.12.1/IdInfo.html#t%3AIdDetails