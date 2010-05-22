{-# LANGUAGE ViewPatterns, PatternGuards,
             GeneralizedNewtypeDeriving #-}
-- | Translate GHC Core into (high-level) GRIN.
--
-- GHC Core is typed, allows arbitrary expressions as function
-- arguments, and allows local functions.
--
-- GRIN is untyped (for now), requires all function arguments to be
-- variables (or at atoms), and only allows top-level functions.
--
-- GHC Core uses @case@ expressions for both evaluation and branching,
-- whereas GRIN uses @case@ only for branching and uses a special
-- @eval@ instruction for evaluation.
-- 
--
-- The main workers are 'buildWhnf' and 'buildThunk'.
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
import Lambdachine.Id as N
import Lambdachine.Grin as Grin
import Lambdachine.Builtin

import qualified Var as Ghc
import qualified VarEnv as Ghc
import qualified HscTypes as Ghc ( CoreModule(..) )
import qualified Module as Ghc
import qualified Literal as Ghc
import qualified Name as Ghc
import qualified IdInfo as Ghc
import qualified DataCon as Ghc
import qualified CoreSyn as Ghc ( Expr(..) )
import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr,
                 Bind(..), Expr(Lam, Let, Type, Cast, Note),
                 AltCon(..),
                 collectBinders, flattenBinds, collectArgs )
import Var ( isTyVar )

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Reader
import Data.Char ( ord )
import Data.Either ( partitionEithers )
import qualified Data.Map as M
import qualified Data.Set as S

-- | The transformation monad.
--
newtype Trans a = Trans (ReaderT TransEnv (State TransState) a)
  deriving (Functor, Applicative, Monad, MonadState TransState,
            MonadReader TransEnv)

type TransEnv = ()

newtype IdEnv a = IdEnv (M.Map Id a)
  deriving (Eq, Ord, Show)

data TransState = TransState
  { tsUniques :: Supply Unique
  , tsNameMap :: Ghc.IdEnv Grin.Var
  , tsCafs :: S.Set Grin.Var
  }

modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do s <- get; put $! f s

-- emptyIdEnv :: IdEnv a
-- emptyIdEnv = IdEnv M.empty

runTrans :: Supply Unique -> Trans a -> a
runTrans s (Trans m) = evalState (runReaderT m env0) state0
 where
   env0 = ()
   state0 = TransState { tsUniques = s
                       , tsNameMap = initialNameMap
                       , tsCafs = S.empty }

initialNameMap :: Ghc.IdEnv Grin.Var
initialNameMap =
  Ghc.mkVarEnv [(Ghc.falseDataConId, falseDataConId)
               ,(Ghc.trueDataConId, trueDataConId)]

-- | Converts a 'Ghc.CoreModule' to our GRIN IR.
--
-- Performs no optimisations.
--
toGrinModule :: Supply Unique -> Ghc.CoreModule -> Grin.Module
toGrinModule uniques core_mdl =
  Grin.Module
    { moduleId = toGrinModuleId (Ghc.cm_module core_mdl)
    , moduleFuns = funs
    , moduleCafs = cafs }
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

toVar :: (N.Name -> Grin.Var) -> Ghc.Var -> Grin.Var
toVar f gvar = f (N.name $! show gvar)

toDataCon :: Ghc.DataCon -> Grin.DataCon
toDataCon dcon = N.mkDataConId $ N.name $! Ghc.getOccString dcon

freshVar :: Show a => a -> Trans Grin.Var
freshVar var = do
  s <- gets tsUniques
  case split2 s of
    (u, s') -> do
      modify' $ \st -> st{ tsUniques = s' }
      return $ N.mkLocalId (N.freshName u $! show var)

translateId :: (N.Name -> Grin.Var) -> Ghc.Id -> Trans Grin.Var
translateId f nm = do
  env <- gets tsNameMap
  case Ghc.lookupVarEnv env nm of
    Just grin_var -> return grin_var
    Nothing -> do
      s <- gets tsUniques
      case split2 s of
        (u, s') -> do
        let f' = case Ghc.idDetails nm of
                   Ghc.DataConWorkId _ -> N.mkDataConId
                   Ghc.DataConWrapId _ -> N.mkDataConId
                   Ghc.PrimOpId _ -> N.mkPrimOpId
                   _ -> f
        let grin_var = f' (N.freshName u $! show nm)
        modify' $ \st ->
          st{ tsUniques = s'
            , tsNameMap =
                Ghc.extendVarEnv (tsNameMap st) nm grin_var }
        return grin_var

addAlias :: Ghc.Id -> Grin.Var -> Trans ()
addAlias var gvar = do
  modify' $ \st ->
    st{ tsNameMap = Ghc.extendVarEnv (tsNameMap st) var gvar }

type Caf = (Grin.Var, Grin.Expr)

toGrinFuns :: [CoreBind] -> Trans ([Caf], [FunDef])
toGrinFuns binds_ = partitionEithers <$> walkBind binds_ []
 where
   walkBind :: [CoreBind] -> [Either Caf FunDef]
            -> Trans [Either Caf FunDef]
   walkBind [] cafs_or_funs = return $ reverse cafs_or_funs
   walkBind (NonRec f e : binds) cafs_or_funs = do
     f' <- translateId N.mkTopLevelId f
     r <- translateOne f' e
     walkBind binds (r : cafs_or_funs)

   walkBind (Rec bs : binds) cafs_or_funs = do
     f's <- mapM (translateId N.mkTopLevelId . fst) bs
     let go [] cafs_or_funs = walkBind binds cafs_or_funs
         go ((f', (_, e)) : bs') cafs_or_funs = do
           r <- translateOne f' e
           go bs' (r : cafs_or_funs)
     go (zip f's bs) cafs_or_funs

   translateOne f' e =
     let e' = eraseTypes e in
     case collectBinders e' of
       ([], body) -> do
         -- it's a TRAP! .. er .. a CAF
         modify' $ \st -> st{ tsCafs = S.insert f' (tsCafs st) }
         Left <$> toGrinCaf f' body
       (args, body) -> do
         args' <- mapM (translateId N.mkLocalId) args
         Right <$> toGrinFun f' args' body

isLambda :: CoreExpr -> Bool
isLambda (Lam _ _) = True
isLambda _ = False
{-
toGrinFunOrCaf :: (CoreBndr, CoreExpr) -> Trans (Either Caf FunDef)
toGrinFunOrCaf (toVar N.mkTopLevelId -> f, collectBinders -> (args_, body)) =
  let args = filter (not . isTyVar) args_ in
  case args of
    [] -> Left <$> toGrinCaf f body
    _ -> Right <$> toGrinFun f (map (toVar N.mkLocalId) args) body
-}
toGrinCaf :: Grin.Var -> CoreExpr -> Trans Caf
toGrinCaf f e =
  (,) f <$> toGrinExpr e

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

updFlag :: Grin.Var -> Trans UpdFlag
updFlag x = do
  is_whnf <- isKnownWhnf x
  if is_whnf then return NeverUpdate
   else return AlwaysUpdate

-- | Is the given Id known to be in WHNF.
--
-- This is a case if it's a constructor, a primop, or a top-level
-- function (but not a CAF).
isKnownWhnf :: Grin.Var -> Trans Bool
isKnownWhnf x
  | N.isDataConId x || N.isPrimOpId x = return True
  | N.isTopLevelId x = do
     cafs <- gets tsCafs
     if x `S.member` cafs then return False else return True
  | otherwise = return False

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
buildWhnf (Ghc.Var x) = do
  x' <- translateId N.mkLocalId x
  b <- isKnownWhnf x'
  if b then return (Unit (Var x'))
    else return (Eval AlwaysUpdate x')
buildWhnf e@(Ghc.App _ _) = do
  let (fun, args) = collectArgs e
  buildThunks args $ \as ->
    case fun of
      Ghc.Var f_ -> do
        f <- translateId N.mkLocalId f_
        if N.isPrimOpId f then return $ Prim f as else
         if isDataConId f then return $ Unit (Node f as) else
          return $ App f as
      _ -> do
        fun' <- buildWhnf fun
        f <- freshVar (text "f")
        return (fun' :>>= (Var f :-> App f as))
buildWhnf (Let (NonRec v e1) e2) = do -- allocation
  v' <- freshVar v
  e1' <- buildThunk e1
  e2' <- buildWhnf e2
  return (e1' :>>= (Var v' :-> e2'))
buildWhnf (Ghc.Case e1 v _ty alts) = do
  v' <- translateId N.mkLocalId v
  e1' <- buildWhnf e1
  alts' <- forM alts $ \(con, xs, e2) -> do
    (:>) <$> pat_to_val v' con xs
         <*> buildWhnf e2
  case alts' of
    [(pat :> e2')] ->
      -- case with only one alternative = strict let
      return (e1' :>>= (Var v' :-> Unit (Var v') :>>= (pat :-> e2')))
    _ ->
      return $ e1' :>>= (Var v' :-> Case v' alts')
buildWhnf _ = return $ Unit Void

-- | Build a thunk corresponding to the input expression.
buildThunk :: CoreExpr -> Trans Grin.Expr
buildThunk (Ghc.Var x) =
  Unit . Var <$> translateId N.mkLocalId x
buildThunk e@(Ghc.Lit _) = buildWhnf e
buildThunk e@(Ghc.App _ _) = do
  let (fun, args) = collectArgs e
  buildThunks args $ \as -> do
    let eval_fun = do
          fun' <- buildWhnf fun
          f <- freshVar (text "f")
          return $ fun' :>>= (Var f :-> Store (Node f as))
    case fun of
      Ghc.Var f_ -> do
        f <- translateId N.mkLocalId f_
        if N.isPrimOpId f then return $ Prim f as else
         if N.isDataConId f then return $ Store $ Node f as else do
          b <- isKnownWhnf f
          if b then return (Store $ Node f as) else eval_fun
      _ -> eval_fun

buildThunk _ = return $ Unit Void

pat_to_val :: Var -> AltCon -> [CoreBndr]
           -> Trans Value
pat_to_val x DEFAULT [] = return $ Var x
pat_to_val x (DataAlt dcon) xs =
  Node <$> translateId N.mkDataConId (Ghc.dataConWrapId dcon)
       <*> mapM (translateId N.mkLocalId) xs
pat_to_val x (LitAlt l) [] =
  return (Lit (toGrinLiteral l))

buildThunks :: [CoreExpr] -> ([Var] -> Trans Grin.Expr)
            -> Trans Grin.Expr
buildThunks es_ kont = go es_ []
 where
   go [] xs = kont (reverse xs)
   go ((Ghc.Var x):es) xs = do
     x' <- translateId N.mkLocalId x
     go es (x' : xs)
   go (e:es) xs = do
     e' <- buildThunk e
     x <- freshVar (text "t")
     es' <- go es (x:xs)
     return $ e' :>>= (Var x :-> es')

-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
{-
isDataConId :: Ghc.Id -> Bool
isDataConId x = case Ghc.idDetails x of
  Ghc.DataConWorkId _ -> True
  Ghc.DataConWrapId _ -> True
  _ -> False

isPrimOp :: Ghc.Id -> Bool
isPrimOp x = case Ghc.idDetails x of
  Ghc.PrimOpId _ -> True
  _ -> False
-}
-- -------------------------------------------------------------------

tidyGrin :: Grin.Module -> Grin.Module
tidyGrin m = m

-- file:///Users/ts319/local/share/doc/ghc/html/libraries/ghc-6.12.1/IdInfo.html#t%3AIdDetails
