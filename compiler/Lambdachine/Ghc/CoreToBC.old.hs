{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving #-}
module Lambdachine.Ghc.CoreToBC where

import Lambdachine.Utils
import Lambdachine.Id as N
import Lambdachine.Grin.Bytecode as Grin
import Lambdachine.Builtin
import Lambdachine.Utils.Unique ( mkBuiltinUnique )

import qualified Var as Ghc
import qualified VarEnv as Ghc
import qualified HscTypes as Ghc ( CoreModule(..) )
import qualified Module as Ghc
import qualified Literal as Ghc
import qualified Name as Ghc
import qualified IdInfo as Ghc
import qualified DataCon as Ghc
import qualified CoreSyn as Ghc ( Expr(..) )
import qualified PrimOp as Ghc
import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId )
import Outputable ( showPpr )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr, CoreArg, CoreAlt,
                 Bind(..), Expr(Lam, Let, Type, Cast, Note),
                 AltCon(..),
                 collectBinders, flattenBinds, collectArgs )
import Var ( isTyVar )
import Unique ( Uniquable(..), getKey )

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Char ( ord )
import Data.Either ( partitionEithers )

import Debug.Trace

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
  , tsNameMap :: Ghc.IdEnv ValueLocation
    -- ^ Maps GHC Ids to their location in bytecode.
    --
    -- For example, when translating the body of function @f x y@,
    -- this will map @x@ to @FixedReg 0@ and @y@ to @FixedReg 1@.
    -- This corresponds to the calling convention.
    --
  , tsCafs :: S.Set Id
  , tsInstrs :: [InstrA]
  , tsLocalBCOs :: M.Map Id (BytecodeObject BcVar [InstrA])
  }

-- | Describes where to find the value of a variable.
--
-- See 'tsNameMap' for more information.
data ValueLocation
  = InVar BcVar
    -- ^ The value has already been loaded into the given register.
  | Field BcVar Int
    -- ^ The value can be loaded from memory by loading the nth slot
    -- from the given variable.
  | InReg Int
    -- ^ The value is in a specific register.
  | FreeVar Int
  | Global Id

data BcVar = BcVar !Id
           | BcReg Int

instance Pretty BcVar where
  ppr (BcVar x) = ppr x
  ppr (BcReg n) = char 'R' <> ppr n

type InstrA = BcInstr BcVar BcConst

-- emptyIdEnv :: IdEnv a
-- emptyIdEnv = IdEnv M.empty

runTrans :: Supply Unique -> Trans a -> a
runTrans s (Trans m) = evalState (runReaderT m env0) state0
 where
   env0 = ()
   state0 = TransState { tsUniques = s
                       , tsNameMap = initialNameMap
                       , tsCafs = S.empty
                       , tsInstrs = []
                       , tsLocalBCOs = M.empty }

initialNameMap :: Ghc.IdEnv ValueLocation
initialNameMap =
  Ghc.mkVarEnv [(Ghc.falseDataConId, Global falseDataConId)
               ,(Ghc.trueDataConId, Global trueDataConId)]

emit :: InstrA -> Trans ()
emit i = modify' $ \st -> st{ tsInstrs = i : tsInstrs st }

freshVar :: String -> Trans Id
freshVar base_name = do
  u <- gets tsUniques
  case split2 u of
    (s, u') -> do
      let n = freshName s base_name
      modify' $ \st -> st{ tsUniques = u' }
      return $ mkLocalId n

--------------------------------------------------------------

{-

The input is expected to be in "CorePrep" normal form.  This is the
form that GHC uses before going through the STG + CodeGen pipelines.
It transforms Core into A-normal form and makes strict evaluation
explicit via case expressions.

The following grammar describes the output of CorePrep:

@
Trivial expressions 
   triv ::= lit |  var  | triv ty  |  /\a. triv  |  triv |> co

Applications
   app ::= lit  |  var  |  app triv  |  app ty  |  app |> co

Expressions
   body ::= app  
          | let(rec) x = rhs in body     -- Boxed only
          | case body of pat -> body
          | /\a. body
          | body |> co

Right hand sides (only place where lambdas can occur)
   rhs ::= /\a.rhs  |  \x.rhs  |  body
@

-}


primOpToBinOp :: Ghc.Id -> Maybe (BinOp, OpTy)
primOpToBinOp var | Ghc.PrimOpId primop <- Ghc.idDetails var =
  case primop of
    Ghc.IntAddOp -> Just (PrAdd, Int32Ty)
    Ghc.IntSubOp -> Just (PrSub, Int32Ty)
    Ghc.IntMulOp -> Just (PrMul, Int32Ty)
    Ghc.IntQuotOp -> Just (PrDiv, Int32Ty)
    -- TODO: treat conditionals specially?
    Ghc.IntGtOp -> Just (PrGt, Int32Ty)
    Ghc.IntGeOp -> Just (PrGe, Int32Ty)
    Ghc.IntEqOp -> Just (PrEq, Int32Ty)
    Ghc.IntNeOp -> Just (PrNe, Int32Ty)
    Ghc.IntLtOp -> Just (PrLt, Int32Ty)
    Ghc.IntLeOp -> Just (PrLe, Int32Ty)
    _ -> Nothing
primOpToBinOp _ = Nothing

bcBind :: CoreBndr -> CoreExpr -> Trans [InstrA]
bcBind f rhs = do
  (_,_,is,r) <- bcGenRhs rhs
  return (is ++ [Ret1 r])

type BCOs = M.Map Id (BytecodeObject BcVar BcConst)

bcBindTopLvl :: [CoreBind] -> Trans BCOs
bcBindTopLvl binds0 = go binds0 M.empty 
 where
   go (NonRec f rhs : binds) acc = do
     is <- bcBind f rhs
     go binds $! M.insert (toplevelId f) (mk_bco is) acc
   go (Rec bs : binds) acc0 = go' bs acc0
    where
      go' ((f, rhs) : bs') acc = do
        is <- bcBind f rhs
        go' bs' $! M.insert (toplevelId f) (mk_bco is) acc
      go' [] acc = go binds acc
   go [] acc = return acc
   mk_bco is = BytecodeObject
                 { bcoCode = is
                 , bcoGlobalRefs = []
                 , bcoConstants = [] }

data EvalContext
  = RetCtx
  | BindCtx (BcVar -> Trans ())
  | CaseCtx (BcVar -> Trans ())

bcExpr :: CoreExpr -> Trans ()
bcExpr expr_ = bcGenExpr expr_ >> return ()

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond then_act else_act = do
  b <- cond
  if b then then_act else else_act

-- | Translate a 'CoreExpr' into bytecode in a strict context.
--
-- Returns a sequence of instruction and the name of the virtual
-- register that holds the value.
bcGenExpr :: CoreExpr -> Trans ([InstrA], BcVar)
--bcGenExpr e | trace ("*** " ++ showPpr e) False = undefined
bcGenExpr (Ghc.Lit l) = loadLiteral l
bcGenExpr (Ghc.Var var) = do
  (is, r, eval'd) <- loadVar var
  let is' | eval'd = is
          | otherwise = is ++ [Eval r r]
  return (is', r)
bcGenExpr expr@(Ghc.App _ _) = bcGenApp f args
  where (f, args) = viewGhcApp expr
bcGenExpr (Ghc.Case scrut bndr _ty [(altcon, vars, body)]) = do
  (instrs, r) <- bcGenExpr scrut
  recordLocation bndr (InVar r)
  (instrs', r') <- withMatchEnv r altcon vars $ bcGenExpr body
  return (instrs ++ instrs', r')
bcGenExpr (Ghc.Case scrut bndr _ty alts) = do
  (instrs, r) <- bcGenExpr scrut
  recordLocation bndr (InVar r)
  rslt <- BcVar <$> freshVar "rslt"
  alt_code <- bcGenCases r rslt alts
  return (instrs ++ [Case r alt_code], rslt)
bcGenExpr (Ghc.Let (NonRec x app@(Ghc.App _ _))) =
  case viewGhcApp app of
    (f, args) | isGhcConWorkId f
      -> do (is, r) <- emitStore f args
            recordLocation x (InVar r)
            return (is, r)
bcGenExpr (Ghc.Let _ e) = bcGenExpr e
bcGenExpr (Ghc.Note _ e) = bcGenExpr e
bcGenExpr (Ghc.Cast e _) = bcGenExpr e
bcGenExpr (Ghc.Lam a e) | isTyVar a = bcGenExpr e  -- type abstraction is fine
bcGenExpr r = error $ "bcGenExpr: " ++ showPpr r

isGhcConWorkId :: CoreBndr -> Bool
isGhcConWorkId x
  | Ghc.DataConWorkId _ <- Ghc.idDetails x = True
  | otherwise                              = False

emitStore :: CoreBndr -> [CoreArg] -> Trans ([InstrA], BcVar)
emitStore f args | isGhcConWorkId f = do
  (is1, freg, _) <- loadVar f
  (is2, arg_regs) <- bcGenArgs args
  rslt <- BcVar <$> freshVar "p"
  return (is1 ++ is2 ++ [Store rslt freg arg_regs], rslt)

-- third argument: known to be evaluated
loadVar :: CoreBndr -> Trans ([InstrA], BcVar, Bool)
loadVar var = do
  loc <- Ghc.lookupVarEnv <$> gets tsNameMap <*> pure var
  case loc of
    Nothing 
      | Ghc.DataConWorkId _ <- Ghc.idDetails var 
      -> do
        let g = dataConId var
        x <- BcVar <$> freshVar "con"
        recordLocation var (InVar x)
        return ([LoadG x g], x, True)
      | otherwise -> do
        let g = toplevelId var
        x <- BcVar <$> freshVar "top"
        recordLocation var (InVar x)
        return ([LoadG x g], x, True)
--          error $ "bcGenExpr: free variable: " ++ show var
--     case Ghc.idDetails var of
--         Ghc.DataConWrapId _ -> error "unimplemented"
    Just (InVar x) -> return ([], x, False)
    Just (InReg r) -> return ([], BcReg r, False)
    Just (Field p n) -> do
      x <- BcVar <$> freshVar "fld"
      recordLocation var (InVar x)
      return ([Fetch x p n], x, False)
    Just (Global g) -> do
      x <- BcVar <$> freshVar "gbl"
      recordLocation var (InVar x)
      return ([LoadG x g], x, True)  -- FIXME: not if CAF

--bcGenRhs :: CoreExpr -> 
bcGenApp :: CoreBndr -> [CoreArg] -> Trans ([InstrA], BcVar)
bcGenApp f [] = bcGenExpr (Ghc.Var f)
bcGenApp f args@[a1,a2]  -- binary primop
  | Ghc.PrimOpId p <- Ghc.idDetails f
  , Just (op, ty) <- primOpToBinOp f = do
    (is, [r1, r2]) <- bcGenArgs args
    r <- BcVar <$> freshVar "prim"
    return (is ++ [BinR op ty r r1 r2], r)
bcGenApp f args -- other primop
  | Ghc.PrimOpId p <- Ghc.idDetails f = error "Unsupported primop"
bcGenApp f args 
  | isGhcConWorkId f = emitStore f args
bcGenApp f args = do
  (fis, freg) <- bcGenExpr (Ghc.Var f)
  (is, argregs) <- bcGenArgs args
  r <- BcVar <$> freshVar "app"
  return (fis ++ is ++ [Call False r freg argregs],  r)

bcGenArgs :: [CoreArg] -> Trans ([InstrA], [BcVar])
bcGenArgs args0 = go args0 [] []
 where
   go [] instrs vars = return (instrs, reverse vars)
   go (arg:args) instrs regs = do
     (is, r) <- bc_arg arg
     go args (instrs ++ is) (r:regs)

   bc_arg (Ghc.Lit l) = loadLiteral l
   bc_arg (Ghc.Var var) = do
     (is, r, _) <- loadVar var
     return (is, r)
   bc_arg (Ghc.App x (Ghc.Type _)) = bc_arg x
   bc_arg (Lam a x) | isTyVar a = bc_arg x
   bc_arg (Cast x _) = bc_arg x
   bc_arg (Note _ x) = bc_arg x


type FreeVars = [Ghc.Id]

bcGenRhs :: CoreExpr -> Trans (Int, FreeVars, [InstrA], BcVar)
bcGenRhs (viewGhcLam -> (params, body)) = do
  --r <- BcVar <$> freshVar "rhs"
  forM_ (zip params [0..]) $ \(x,n) ->
    recordLocation x (InReg n)
  (is, r) <- bcGenExpr body
  return (1, [], is, r)

bcGenCases :: 
     BcVar -- ^ The thing we're matching on.
  -> BcVar -- ^ Each case alternative should write its result into
           -- this variable.
  -> [CoreAlt]
  -> Trans [(Int, [InstrA])]
bcGenCases obj rslt alts =
  forM alts $ \(altcon, vars, body) -> do
    (instrs, r) <- withMatchEnv obj altcon vars (bcGenExpr body)
    -- TODO: move r into rslt
    return (0, instrs)

withMatchEnv :: BcVar -> AltCon -> [CoreBndr] -> Trans a -> Trans a
withMatchEnv _obj DEFAULT [] m = m
withMatchEnv _obj (LitAlt _) [] m = m
withMatchEnv obj (DataAlt _) vars m =
  withLocs [ (x, Field obj n) | (x,n) <- zip vars [1..] ] m

recordLocation :: Ghc.Id -> ValueLocation -> Trans ()
recordLocation var loc =
  modify' $ \st -> 
    st{ tsNameMap = Ghc.extendVarEnv (tsNameMap st) var loc }

withLocs :: [(Ghc.Id, ValueLocation)] -> Trans a -> Trans a
withLocs locs body = do
  st <- get
  modify' $ \st ->
    st{ tsNameMap = Ghc.extendVarEnvList (tsNameMap st) locs }
  a <- body
  put st
  return a

loadLiteral :: Ghc.Literal -> Trans ([InstrA], BcVar)
loadLiteral (Ghc.MachInt n) = do
  (,) [] <$> (BcVar <$> freshVar "lit")

-- | View expression as n-ary application.  The function must be a
-- variable.  Ignores type abstraction, notes and coercions.
viewGhcApp :: CoreExpr -> (CoreBndr, [CoreArg])
viewGhcApp expr = go expr []
 where
   go (Ghc.Var v)          as = (v, as)
   go (Ghc.App f (Type _)) as = go f as
   go (Ghc.App f a)        as = go f (a:as)
   go (Ghc.Note _ e)       as = go e as
   go (Ghc.Cast e _)       as = go e as
   go (Ghc.Lam x e) as | isTyVar x = go e as
   go _ _ = error $ "viewGhcApp: " ++ showPpr expr

-- | View expression as n-ary abstraction.  Ignores type abstraction.
viewGhcLam :: CoreExpr -> ([CoreBndr], CoreExpr)
viewGhcLam expr = go expr []
 where
   go (Ghc.Lam x e) xs
     | isTyVar x = go e xs
     | otherwise = go e (x:xs)
   go e xs = (reverse xs, e)

dataConId :: Ghc.Id -> Id
dataConId x | Ghc.DataConWorkId _ <- Ghc.idDetails x =
  mkDataConId (N.mkBuiltinName (fromGhcUnique x) (Ghc.getOccString x))

toplevelId :: Ghc.Id -> Id
toplevelId x = --  | Ghc.VanillaId <- Ghc.idDetails x =
  mkTopLevelId (N.mkBuiltinName (fromGhcUnique x) (Ghc.getOccString x))

fromGhcUnique :: Uniquable a => a -> Unique
fromGhcUnique x = mkBuiltinUnique (getKey (getUnique x))

{-
evalWhnf :: CoreExpr -> EvalContext -> Trans ()
evalWhnf (Ghc.Lit l) ctx = do
  v <- freshVar "lit"
  loadLiteral l v
  case ctx of
    RetCtx -> emit (Ret1 v)
    BindCtx k -> k v
    CaseCtx k -> k v
evalWhnf (Ghc.Var x) RetCtx = do
  x' <- translateId mkLocalId x
  ifM (isKnownWhnf x')
    (do mumble (fresh $ \v -> emit (LoadG v x'))
  if known_whnf then
    
  if isDataConId x' || isTopLevelId x' then do
    v <- freshVar "gbl"
    emit $ LoadG v x'
    emit $ Ret1 v
   else do
    dummy <- freshVar "dummy"
    emit $ Eval dummy x'
    emit $ Ret1 x'
evalWhnf (Ghc.Var x) (BindCtx k) = do
  x' <- translateId mkLocalId x
  if isDataConId x' || isTopLevelId x' then do
    v <- freshVar "gbl"
    emit $ LoadG v x'
    k v
   else
    k x'
evalWhnf e@(Ghc.App _ _) RetCtx = do -- tailcall
  let (fun, args) = collectArgs e
  buildThunks args $ \arg_vars ->
    evalWhnf fun $ BindCtx $ \f ->
      emit $ Call True f arg_vars

buildThunks :: [CoreExpr] -> ([BcVar] -> Trans a) -> Trans a
buildThunks exprs k = go exprs []
  where
    go [] acc = k (reverse acc)
    go (e:es) acc = do
      v <- buildThunk e
      go es (v:acc)

buildThunk :: CoreExpr -> Trans BcVar
buildThunk (Ghc.Var x) = translateId mkLocalId x
buildThunk (Ghc.Lit l) = do
  v <- freshVar "lit"
  loadLiteral l v
  return v
buildThunk e@(Ghc.App _ _) = do
  

-- | Load literal into specified variable.
loadLiteral :: Ghc.Literal -> BcVar -> Trans ()
loadLiteral (Ghc.MachInt n) x =
  emit $ LoadK x (CInt n)
loadLiteral l _ =
  error $ "Literal type not yet supported: " ++ show l

translateId :: (Name -> BcVar) -> Ghc.Id -> Trans BcVar
translateId f ident = do
  env <- gets tsNameMap
  case Ghc.lookupVarEnv env ident of
    Just x -> return  x
    Nothing -> do
      s <- gets tsUniques
      case split2 s of
        (u, s') -> do
        let f' = case Ghc.idDetails ident of
                   Ghc.DataConWorkId _ -> N.mkDataConId
                   Ghc.DataConWrapId _ -> N.mkDataConId
                   Ghc.PrimOpId _ -> N.mkPrimOpId
                   _ -> f
        let grin_var = f' (N.freshName u $! show ident)
        modify' $ \st ->
          st{ tsUniques = s'
            , tsNameMap =
                Ghc.extendVarEnv (tsNameMap st) ident grin_var }
        return grin_var
-}


