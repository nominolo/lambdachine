{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambdachine.Ghc.StgToBytecode ( stgToBytecode ) where

import Lambdachine.Builtin
import Lambdachine.Ghc.Utils
import Lambdachine.Grin.Bytecode as Grin
import Lambdachine.Grin.Analyse ( isVoid )
import Lambdachine.Id as N
import Lambdachine.Utils hiding ( Uniquable(..) )
import Lambdachine.Utils.Unique ( mkBuiltinUnique )

import qualified Var as Ghc
import qualified VarEnv as Ghc
import qualified VarSet as Ghc
import qualified HscTypes as Ghc ( CgGuts(..) )
import qualified Module as Ghc
import qualified Literal as Ghc
import qualified Name as Ghc hiding ( varName )
import qualified IdInfo as Ghc
import qualified Id as Ghc
import qualified Type as Ghc
import qualified DataCon as Ghc
import DataCon ( DataCon, dataConWorkId, dataConRepType, dataConOrigResTy )
import qualified CoreSyn as Ghc ( Expr(..), mkConApp, isTypeArg )
import qualified PrimOp as Ghc
import qualified TysWiredIn as Ghc
import qualified TysPrim as Ghc
import qualified TyCon as Ghc
import qualified TypeRep as Ghc
import qualified Outputable as Ghc
import qualified MkId as Ghc ( realWorldPrimId )
import qualified CoreUtils as Ghc
import qualified Coercion as Ghc
import TyCon ( TyCon )
import Pair ( Pair(..) )
import Outputable ( Outputable, alwaysQualify, showSDocOneLine )
-- import DynFlags ( tracingDynFlags )
import qualified Outputable as Out
import qualified Pretty as Out
import CoreSyn ( CoreBndr, AltCon(..) )
import StgSyn
import Var ( isTyVar )
import Unique ( Uniquable(..), getKey )
import FastString ( unpackFS )

import qualified Data.ByteString.Char8 as BC8
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative hiding ( (<*>) )
import Control.Monad.State
import Control.Monad.Reader
--import Control.Monad.Fix
import Data.Foldable ( toList )
import Data.List ( foldl', sortBy, partition )
import Data.Ord ( comparing )
import Data.Monoid
import Data.Maybe ( fromMaybe )

import Debug.Trace

#include "../../Opcodes.h"

------------------------------------------------------------------------
-- * Debug Utils:

unimplemented :: String -> a
unimplemented str = error $ "UNIMPLEMENTED: " ++ str

invariant :: Applicative m => String -> Bool -> m ()
invariant msg check =
  if not check then error ("FATAL: " ++ msg) else pure ()

-- tracePpr :: Outputable a => a -> b -> b
-- tracePpr o exp = trace (">>> " ++ showPpr o) exp

-- showPpr1 :: Outputable a => a -> String
-- showPpr1 o = Out.showDocWith Out.OneLineMode $
--   Out.withPprStyleDoc tracingDynFlags Out.defaultUserStyle (Out.ppr o)

------------------------------------------------------------------------
-- * Other utilities

-- | More efficient version of @length xs == n@.
lengthIs :: [a] -> Int -> Bool
lengthIs [] 0 = True
lengthIs (_:xs) n = lengthIs xs (n - 1)
lengthIs _ _  = False

------------------------------------------------------------------------
-- * Top-level Interface
type Bcis x = BcGraph O x

stgToBytecode :: GlobalEnv
              -> Supply Unique
              -> Ghc.ModuleName
              -> [StgBinding]
              -> [TyCon]
              -> BCOs
stgToBytecode env uniques mdl bndrs0 data_tycons =
  runTrans env mdl uniques $ do
    mapM_ transTyCon data_tycons
    mapM_ transTopLevelBind bndrs0
    getBCOs

------------------------------------------------------------------------

transTyCon :: TyCon -> Trans ()
transTyCon tycon = do
  env <- askGlobalEnv
  addBCO (tyConId env (Ghc.tyConName tycon)) $
    BcTyConInfo{ bcoDataCons =
                    map (dataConInfoTableId env) (Ghc.tyConDataCons tycon) }
  forM_ (Ghc.tyConDataCons tycon) $ \dcon -> do
    let dcon_id = dataConInfoTableId env dcon
        -- ty = transType (Ghc.dataConRepType dcon)
        arg_tys = map (transType env) (Ghc.dataConRepArgTys dcon)
          -- | FunTy args _ <- ty = args
                -- | otherwise = []
        bco = BcConInfo { bcoConTag = Ghc.dataConTag dcon
                        , bcoConFields = Ghc.dataConRepArity dcon
                        , bcoConArgTypes = arg_tys }
    -- error $ "TODO: dcon type: " ++ showPpr (dcon, Ghc.dataConRepType dcon)
    --    ++ " " ++ pretty arg_tys
    addBCO dcon_id bco

------------------------------------------------------------------------
-- The bytecode translation generation monad

newtype Trans a = Trans { unTrans :: ReaderT GlobalEnv (State TransState) a }
  deriving (Functor, Applicative, Monad, MonadFix)

data TransState = TransState
  { tsUniques    :: Supply Unique
  , tsLocalBCOs  :: BCOs
  , tsModuleName :: !Ghc.ModuleName
  , tsParentFun  :: Maybe String
  , tsStack      :: [StgExpr]
  }

runTrans :: GlobalEnv -> Ghc.ModuleName -> Supply Unique -> Trans a -> a
runTrans env mdl us (Trans m) = evalState (runReaderT m env) s0
 where
   s0 = TransState { tsUniques = us
                   , tsLocalBCOs = M.empty
                   , tsModuleName = mdl
                   , tsParentFun = Nothing
                   , tsStack = [] }

askGlobalEnv :: Trans GlobalEnv
askGlobalEnv = Trans $ ask

genUnique :: Trans (Supply Unique)
genUnique = Trans $ lift $ do
  s <- get
  case split2 (tsUniques s) of
    (us, us') -> do
      put $! s{ tsUniques = us }
      return us'

instance UniqueMonad Trans where
  freshUnique = hooplUniqueFromUniqueSupply `fmap` genUnique

within :: StgExpr -> Trans a -> Trans a
within e (Trans m) = Trans $ do
  s <- get
  let old = tsStack s
  put $! s{ tsStack = e : old }
  r <- m
  s <- get
  put $! s{ tsStack = old }
  return r

getStack :: Trans [StgExpr]
getStack = Trans $ gets tsStack

getThisModule :: Trans Ghc.ModuleName
getThisModule = Trans $ gets tsModuleName

getBCOs :: Trans BCOs
getBCOs = Trans $ gets tsLocalBCOs

addBCO :: Id -> BytecodeObject -> Trans ()
addBCO x bco = Trans $
  modify' $ \s ->
    let !bcos' = M.insert x bco (tsLocalBCOs s) in
    s{ tsLocalBCOs = bcos' }

-- | Prefix newly generated names with the given 'Ghc.Id'.
--
-- This is used to give local closures more descriptive names.  For
-- example the GHC Core code occurring in module @M@:
--
-- > foo x y = ... let bar z = ... (let quux a = ... in ...) in ...
--
-- will generate three closures named @M.foo@, @M.foo_bar@, and
-- @M.foo_bar_quux@.
withParentFun :: Ghc.Id -> Trans a -> Trans a
withParentFun x (Trans act) = do
  genv <- askGlobalEnv
  Trans $ do
    s <- get
    let x_occ = showSDocForUser genv Ghc.neverQualify (Ghc.ppr (Ghc.getOccName x))
        pfun = tsParentFun s
    put $! (case pfun of
             Nothing ->
               s{ tsParentFun = Just x_occ }
             Just p ->
               s{ tsParentFun = Just (p ++ "_" ++ x_occ) })
    r <- act
    s' <- get
    put $! s'{ tsParentFun = pfun }
    return r

getParentFun :: Trans (Maybe String)
getParentFun = Trans (gets tsParentFun)

------------------------------------------------------------------------

-- | Describes where to find the value of a variable.
data ValueLocation
  = InVar BcVar
    -- ^ The value has already been loaded into the given register.
  | Field BcVar Int Ghc.Type
    -- ^ The value can be loaded from memory by loading the nth slot
    -- from the given variable.
  | InReg Int Ghc.Type
    -- ^ The value is in a specific register.
  | FreeVar Int
    -- ^ The value is a free variable of the current closure at the
    -- given offset.
  | Fwd
    -- ^ A forward reference.
  | Self
    -- ^ The value is the contents of the @Node@ pointer.
  | Global Ghc.Type Id
    -- ^ The value is a top-level ID.
  | Literal Ghc.Type BcConst
    -- ^ The variable is known to have this value.
  | Void
    -- ^ The value does not have a representation.

instance Pretty ValueLocation where
  ppr (InVar x)     = text "InVar" <+> ppr x
  ppr (Field x n t) = text "Field" <+> ppr x <+> ppr n <+> prettyGhc t
  ppr (InReg n t)   = text "InReg" <+> ppr n <+> prettyGhc t
  ppr (FreeVar n)   = text "FreeVar" <+> ppr n
  ppr Fwd           = text "Fwd"
  ppr Self          = text "Self"
  ppr (Global _ x)  = text "Global" <+> ppr x
  ppr Void          = text "Void"

-- | Maps GHC Ids to their (current) location in bytecode.
--
-- For example, when translating the body of function @f x y@,
-- this will map @x@ to @InReg 0@ and @y@ to @InReg 1@.
-- This corresponds to the calling convention.
--
-- We also use this to avoid unnecessary loads.  When pattern matching
-- variable @z@ the beginning of the case alternative for @C x y@ will
-- /not/ immediately load @x@ and @y@ into registers.  Instead we add
-- @x -> Field z 1@ and @y -> Field z 2@ to the @KnownLocs@.  If we
-- later do need @x@ or @y@ we can issue the store there.
--
data KnownLocs = KnownLocs
  { closureLocs :: !(Ghc.IdEnv ValueLocation)
  , itblLocs    :: !(Ghc.IdEnv ValueLocation)
  }

lookupLoc :: KnownLocs -> CoreBndr -> Maybe ValueLocation
lookupLoc (KnownLocs env _) x = Ghc.lookupVarEnv env x

lookupItblLoc :: KnownLocs -> CoreBndr -> Maybe ValueLocation
lookupItblLoc (KnownLocs _ denv) x = Ghc.lookupVarEnv denv x

updateLoc :: KnownLocs -> CoreBndr -> ValueLocation -> KnownLocs
updateLoc (KnownLocs env denv) x l = KnownLocs (Ghc.extendVarEnv env x l) denv

updateItblLoc :: KnownLocs -> CoreBndr -> ValueLocation -> KnownLocs
updateItblLoc (KnownLocs env denv) x l = KnownLocs env (Ghc.extendVarEnv denv x l)

extendLocs :: KnownLocs -> [(CoreBndr, ValueLocation)] -> KnownLocs
extendLocs (KnownLocs env denv) xls =
  KnownLocs (Ghc.extendVarEnvList env xls) denv

noLocs :: KnownLocs
noLocs = KnownLocs Ghc.emptyVarEnv Ghc.emptyVarEnv

-- The local environment always includes `realWorld#`.  It doesn't
-- actually have a runtime representation.
mkLocs :: [(Ghc.Id, ValueLocation)] -> KnownLocs
mkLocs l = KnownLocs (Ghc.extendVarEnv (Ghc.mkVarEnv l) Ghc.realWorldPrimId Void)
                     Ghc.emptyVarEnv

instance Monoid KnownLocs where
  mempty = noLocs
  (KnownLocs e1 d1) `mappend` (KnownLocs e2 d2) =
    KnownLocs (Ghc.plusVarEnv e1 e2) (Ghc.plusVarEnv d1 d2)


------------------------------------------------------------------------

transTopLevelBind :: StgBinding -> Trans ()
transTopLevelBind (StgNonRec x rhs) = do
  env <- askGlobalEnv
  x' <- (\mdl -> toplevelId env mdl x) <$> getThisModule
  transTopLevelRhs x x' rhs
transTopLevelBind (StgRec recs) = do
  env <- askGlobalEnv
  forM_ recs $ \(x, rhs) -> do
    x' <- (\mdl -> toplevelId env mdl x) <$> getThisModule
    transTopLevelRhs x x' rhs

transTopLevelRhs :: Ghc.Id -> Id -> StgRhs -> Trans ()
transTopLevelRhs bndr0 bndr rhs = do
  genv <- askGlobalEnv
  case rhs of
    StgRhsCon _ccs dcon args ->
      transTopLevelRhsConstr bndr dcon args

    StgRhsClosure _ccs bndrInfo frees upd _srt args body -> do
      invariant ("Toplevel closure cannot have any free variables" ++
                 showPpr genv rhs)$
        null frees
      transTopLevelRhsClosure bndr0 bndr bndrInfo upd args body

------------------------------------------------------------------------

transTopLevelRhsConstr :: Id -> DataCon -> [StgArg] -> Trans ()
transTopLevelRhsConstr x dcon args = do
  env <- askGlobalEnv
  invariant ("Datacon arity must match given no. of args: " ++
             showPpr env (dcon, args)) $
    args `lengthIs` Ghc.dataConRepRepArity dcon

  this_mdl <- getThisModule
  let dcon' = dataConInfoTableId env dcon
      fields = transFields (toplevelId env this_mdl) args
  addBCO x $! BcoCon Con dcon' fields

transFields :: (Ghc.Id -> a) -> [StgArg] -> [Either BcConst a]
transFields f args = map to_field args
 where
   to_field (StgVarArg x) = Right (f x)
   to_field (StgLitArg l) = Left $ fromGhcLiteral l

------------------------------------------------------------------------

transTopLevelRhsClosure :: Ghc.Id
                        -> Id
                        -> StgBinderInfo
                        -> UpdateFlag
                        -> [Ghc.Id]
                        -> StgExpr
                        -> Trans ()
transTopLevelRhsClosure x0 x _bndrInfo upd args body = do
  genv <- askGlobalEnv
  let bco_type
        | (_:_) <- args = BcoFun (length args) (map (transType genv . {- Ghc.repType .-}  Ghc.varType) args)
        | otherwise     = CAF

  let env0 = mkLocalEnv [(x, undefined) | x <- args]
      fvi0 = Ghc.emptyVarEnv
      locs0 = mkLocs $ (x0, Self) :
                       [ (b, InReg n t)
                       | (b, n) <- zip args [0..]
                       , let t = repType (Ghc.varType b) ]

  (bcis, locs1, Nothing) <- withParentFun x0 $ within body $
                              transBody body env0 locs0 fvi0 RetC


  g <- finaliseBcGraph bcis
  let bco = BcObject { bcoType = bco_type
                     , bcoCode = g
                     , bcoGlobalRefs = [] -- TODO: toList (globalVars fvs)
                     , bcoConstants = []
                     , bcoFreeVars = M.empty
                     }
  addBCO x bco

------------------------------------------------------------------------

-- | The context describes whether we should bind the result of the
-- translated expression (and to which variable).
--
-- The Type parameter describes the exit shape of the resulting graph.
--
-- If the context is @BindC (Just r)@, then the result should be
-- written into register @r@.  If it is @BindC Nothing@ then the
-- result should be written into a fresh local variable.
data Context x where
  RetC :: Context C
  BindC :: Ghc.Type -> Maybe BcVar -> Context O

contextVar :: Context x -> Maybe BcVar
contextVar RetC = Nothing
contextVar (BindC _ mx) = mx

------------------------------------------------------------------------

freshVar :: String -> (Name -> a) -> Trans a
freshVar nm f = do
  us <- genUnique
  return (f (freshName us (nm ++ tail (show (supplyValue us)))))

mbFreshLocal :: Ghc.Type -> Maybe BcVar -> Trans BcVar
mbFreshLocal _ (Just v) = return v
mbFreshLocal t Nothing = freshVar "%" (\n -> BcVar (mkLocalId n) t)

------------------------------------------------------------------------

-- | Keeps track of non-toplevel variables bound outside the current
-- bytecode context.  Consider the following example:
--
-- > f l y = case l of
-- >           Cons x xs -> let g = <body> in
-- >                        ...
--
-- Assume that @<body>@ mentions @y@ and @x@; these have to become
-- closure variables.  The bytecode for @let g ...@ will look
-- something like this.
--
-- > loadinfo tmp, info-table-for-<body>
-- > alloc tmp, <x>, <y>
--
-- This allocates a closure of size 2, corresponding to the two free
-- variables.  The code for accessing @x@ and @y@ in @<body>@ then has
-- to access them as closure variables, e.g.,
--
-- > loadf r1, 0   ; access x
-- > loadf r3, 1   ; access y
--
-- References to global variables in @<body>@ are accessed as usual
-- using @loadg@ instructions.
--
-- When translating @<body>@ above, this environment contains @{l, y,
-- x, xs}@.
newtype LocalEnv = LocalEnv (Ghc.IdEnv Id)

-- | Lookup element from a 'LocalEnv'.
lookupLocalEnv :: LocalEnv -> Ghc.Id -> Maybe Id
lookupLocalEnv (LocalEnv env) x = Ghc.lookupVarEnv env x

-- | Add a mapping to a 'LocalEnv'.
extendLocalEnv :: LocalEnv -> Ghc.Id -> Id -> LocalEnv
extendLocalEnv (LocalEnv env) x y =
  LocalEnv (Ghc.extendVarEnv env x y)

-- | Create a 'LocalEnv' from a list.
mkLocalEnv :: [(Ghc.Id, Id)] -> LocalEnv
mkLocalEnv lst = LocalEnv (Ghc.mkVarEnv lst)

extendLocalEnvList :: LocalEnv -> [Ghc.Id] -> LocalEnv
extendLocalEnvList (LocalEnv env) xs =
  LocalEnv $ Ghc.extendVarEnvList env [ (x, undefined) | x <- xs ]

-- | Create an empty 'LocalEnv'.  @emptyLocalEnv == mkLocalEnv []@
emptyLocalEnv :: LocalEnv
emptyLocalEnv = LocalEnv Ghc.emptyVarEnv

------------------------------------------------------------------------

-- | Maps free variables to their index in closure.
type FreeVarsIndex = Ghc.IdEnv Int

------------------------------------------------------------------------

transBody :: StgExpr
          -> LocalEnv
          -> KnownLocs
          -> FreeVarsIndex
          -> Context x
          -> Trans (Bcis x, KnownLocs, Maybe BcVar)

transBody (StgLit lit) env locs0 fvi ctxt = do
  (is, r) <- transLiteral lit (contextVar ctxt)
  case ctxt of
    RetC -> return (is <*> insRet1 r, locs0, Nothing)
    BindC _ _ -> return (is, locs0, Just r)

transBody (StgApp x []) env locs0 fvi ctxt = do
  -- variable reference (not a call)
  (is0, r, eval'd, locs1) <- transVar x env fvi locs0 (contextVar ctxt)
  let is | eval'd = is0
         | otherwise = withFresh $ \l ->
                         is0 <*> insEval l r |*><*| mkLabel l
  case ctxt of
    RetC -> return (is <*> insRet1 r, locs1, Nothing)
    BindC _ _ -> return (is, locs1, Just r)

transBody e@(StgApp f args) env locs0 fvi ctxt =
  within e $ transApp f args env locs0 fvi ctxt

transBody (StgOpApp (StgPrimOp primOp) args rslt_ty) env locs0 fvi ctxt = do
  genv <- askGlobalEnv
  (is0, locs1, regs) <- transArgs args env locs0 fvi
  case () of
   _ | Just (op, ty) <- primOpToBinOp primOp, [r1, r2] <- regs
     -> do
       rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
       maybeAddRet ctxt (is0 <*> insBinOp op ty rslt r1 r2)
                   locs1 rslt
   _ | Just (cond, ty) <- isCondPrimOp primOp, [r1, r2] <- regs
     -> do
       -- A comparison op that does not appear within a 'case'.
       -- We must now fabricate a 'Bool' into the result.
       -- That is, `x ># y` is translated into:
       --
       -- >     if x > y then goto l1 else goto l2
       -- > l1: loadlit rslt, True
       -- >     goto l3:
       -- > l2: loadlit rslt, False
       -- > l3:
       rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
       l1 <- freshLabel;  l2 <- freshLabel;  l3 <- freshLabel
       let is1 =  -- shape: O/O
             catGraphsC (is0 <*> insBranch cond ty r1 r2 l1 l2)
               [ mkLabel l1 <*> insLoadGbl rslt (trueDataConId genv)
                            <*> insGoto l3,
                 mkLabel l2 <*> insLoadGbl rslt (falseDataConId genv)
                            <*> insGoto l3]
             |*><*| mkLabel l3
       maybeAddRet ctxt is1 locs1 rslt

   _ | Just (OpNop, [arg_ty], res_ty) <- primOpOther primOp
     -> do
         -- Nop-like-primitives translate into a Move which gets
         -- optimised away by the register allocator (most
         -- likely).
         let [reg] = regs
         result <- mbFreshLocal rslt_ty (contextVar ctxt)
         maybeAddRet ctxt (is0 <*> insMove result reg) locs1 result

   _ | Just (op, arg_tys, res_ty) <- primOpOther primOp
     -> do
       let arity = length arg_tys
       when (arity /= length regs) $
         error $ "Wrong number of primitive args.  Got = "
                 ++ show (length regs) ++ " expected = " ++ show arity
       -- TODO: We could type check the arguments as an extra assertion.
       -- That's a bit tricky given the current setup, though.
       result <- mbFreshLocal rslt_ty (contextVar ctxt)
       maybeAddRet ctxt (is0 <*> insPrimOp op res_ty result regs)
                   locs1 result

   _ | otherwise
     -> do
      st <- getStack
      error $ "Unknown primop: " ++ showPpr genv primOp ++ "\n" ++
        showPpr genv st

transBody (StgConApp dcon []) env locs0 fvi ctxt = do
  -- Constructors without arguments are special.  Since they don't
  -- have a payload, they don't need to be stored on the heap and
  -- therefore we simply return a pointer to the static closure.
  let dcon_closure = dataConWorkId dcon
  (is, r, _, locs1) <- transVar dcon_closure env fvi locs0 (contextVar ctxt)
  maybeAddRet ctxt is locs1 r

transBody (StgConApp dcon args) env locs0 fvi ctxt
 | Ghc.isUnboxedTupleCon dcon
 = do
   genv <- askGlobalEnv
   case ctxt of
     BindC _ _ -> do
       st <- getStack
       error $ "Trying to bind an unboxed tuple to a variable: " ++
         showPpr genv st
     RetC -> do
       (is0, locs1, vars0) <- transArgs args env locs0 fvi
       let vars = removeIf (isVoid genv) vars0
       case vars of
         [] ->
           error "Unboxed tuple contained only void arguments"
         [r] ->
           return (is0 <*> insRet1 r, locs1, Nothing)
         (_:_:_) -> do
           -- Return all N vars in registers r0..r(N-1)
           let resultRegs =
                 [ BcReg n (transType genv (bcVarType var))
                 | (n, var) <- zip [0..] vars ]
           let is =
                 is0 <*> catGraphs [ insMove reg var
                                   | (reg, var) <- zip resultRegs vars ]
           return (is <*> insRetN resultRegs, locs1, Nothing)

 | otherwise = do
  (is0, locs1, regs) <- transArgs args env locs0 fvi
  (is1, locs2, con_reg) <- loadDataCon dcon env fvi locs1 (contextVar ctxt)
   -- trace (showPpr $ dataConOrigResTy dcon) $ do 
  rslt <- mbFreshLocal (dataConOrigResTy dcon) (contextVar ctxt)
  let is2 = (is0 <*> is1) <*> insAlloc rslt con_reg regs
  maybeAddRet ctxt is2 locs2 rslt

transBody e@(StgCase expr _livesWhole _livesRhss bndr _srt altType alts)
    env locs0 fvi ctxt = do
  within e $
    transCase expr bndr altType alts env locs0 fvi ctxt

transBody e@(StgLet binds body) env locs0 fvi ctxt = do
  (is0, locs1, _) <- within e $ transBinds binds env locs0 fvi
  (is1, locs2, mb_r) <- {- withParentFun x $ -} transBody body env locs1 fvi ctxt
  return (is0 <*> is1, locs2, mb_r)

transBody e@(StgLetNoEscape _liveIn _liveRhss binds body)
          env locs0 fvi ctxt = do
  -- TODO: There may be something special we could do with
  -- let-no-escapes.  E.g., access free variables via the stack frame.
  (is0, locs1, _) <- within e $ transBinds binds env locs0 fvi
  (is1, locs2, mb_r) <- transBody body env locs1 fvi ctxt
  return (is0 <*> is1, locs2, mb_r)

transBody (StgSCC _ _ _ _) _env _locs0 _fvi _ctxt = do
  error $ "NYI: Cost centres"

transBody (StgTick _ _ _) _env _locs0 _fvi _ctxt = do
  error $ "NYI: Ticky ticky profiling"

transBody (StgLam _ _) _env _locs0 _fvi _ctxt = do
  error $ "INVARIANT: StgLam must not occur in final STG"

transBody body  _env _locs0 _fvi _ctxt = do
  genv <- askGlobalEnv
  error $ "NYI: Translation of " ++ showPpr genv body

--transBody _ env ctxt = return (emptyGraph, Nothing)

------------------------------------------------------------------------

transBinds :: StgBinding -> LocalEnv -> KnownLocs -> FreeVarsIndex
           -> Trans (Bcis O, KnownLocs, [BcVar])

transBinds (StgNonRec x rhs) env locs0 fvi = do
  (is0, locs1, single_or_alloc, rslt_ty, [])
    <- transRhs rhs x False [] env locs0 fvi
  rslt <- mbFreshLocal rslt_ty Nothing
  let locs2 = updateLoc locs1 x (InVar rslt)
  case single_or_alloc of
    Right (itbl_reg, arg_regs) -> do
      return (is0 <*> insAlloc rslt itbl_reg arg_regs, locs2, [rslt])
    Left clos_reg -> do
      return (is0 <*> insMove rslt clos_reg, locs2, [rslt])

transBinds (StgRec pairs) env locs0 fvi = do
  genv <- askGlobalEnv
  let
    go [] is locs alloced pending_fwd_refs =
      if not (null pending_fwd_refs) then
        error $ "NYI: Forward refs: " ++ 
          unlines [ pretty genv x ++ ": " ++ showFwdRef genv fwd_ref
                  | (x, fwd_ref) <- pending_fwd_refs ]
       else
        return (is, locs, reverse alloced)

    go ((x, rhs):binds) is locs alloced pending_fwd_refs = do
      let fwds_for_x = map fst binds
      let locs1 = extendLocs locs ([ (x, Fwd) | (x, _) <- binds ])
      (is1, locs2, single_or_alloc, rslt_ty, new_fwd_refs) <-
        transRhs rhs x True fwds_for_x env locs1 fvi
      rslt <- mbFreshLocal rslt_ty Nothing
      let locs3 = updateLoc locs2 x (InVar rslt)
      let is' =
            case single_or_alloc of
              Right (itbl_reg, arg_regs) ->
                is <*> is1 <*> insAlloc rslt itbl_reg arg_regs
              Left clos_reg ->
                is <*> is1 <*> insMove rslt clos_reg
      let fwds' = map (\fwd -> (rslt, fwd)) new_fwd_refs ++ pending_fwd_refs
      let (fixable_fwds, fwds'') = partition ((==x) . fwdId . snd) fwds'
      let bcis_fix = [ insStore lhs offs rslt
                     | (lhs, FwdRef offs _) <- fixable_fwds ]

      go binds (is' <*> catGraphs bcis_fix) locs3 (rslt:alloced) fwds''

  go pairs emptyGraph locs0 [] []

data FwdRef = FwdRef
  { fwdOffset :: !Int
  , fwdId     :: !Ghc.Id
  } deriving Eq

showFwdRef :: GlobalEnv -> FwdRef -> String
showFwdRef genv (FwdRef n x) = "Fwd " ++ show n ++ " " ++ showPpr genv x
-- instance Show FwdRef where
--   show (FwdRef n x) = "Fwd " ++ show n ++ " " ++ showPpr x

------------------------------------------------------------------------

type IsRecursive = Bool

transRhs :: StgRhs -> Ghc.Id -> IsRecursive -> [Ghc.Id]
         -> LocalEnv -> KnownLocs -> FreeVarsIndex
         -> Trans (Bcis O, KnownLocs, Either BcVar (BcVar, [BcVar]),
                   Ghc.Type, [FwdRef])
         -- ^ Returns:
         -- 
         --  * The code to load the arguments.
         -- 
         --  * Updated known locations
         -- 
         --  * Either a pointer to the closure or the variable that
         --    holds the info table and he arguments to the allocation
         --    instruction
         -- 
         --  * The type of the result of allocation
         --  * Locations that need to be patched up later since they
         --    referred to values that have not yet been allocated.
transRhs (StgRhsCon _ccs dcon args) self isRec fwds0 env locs00 fvi = do
  let locs0 | isRec     = updateLoc locs00 self Fwd
            | otherwise = locs00
  (is0, locs1, regs) <- transArgs args env locs0 fvi
  (is1, locs2, rdcon) <- loadDataCon dcon env fvi locs1 Nothing
  let rslt_ty = dataConOrigResTy dcon
  let fwds = [ FwdRef n x | (StgVarArg x, n) <- zip args [1..]
                          , x `elem` fwds0 || (isRec && x == self) ]
  return (is0 <*> is1, locs2,
          if null args then Left rdcon else Right (rdcon, regs),
          rslt_ty, fwds)

transRhs r@(StgRhsClosure _ccs _info frees0 _upd _srt args body)
         self isRec fwds0 env locs0_outer fvi_outer = do

  let frees | isRec = filter (/= self) frees0
            | otherwise = frees0
  -- TODO: Detect when args == [] and body is just a function application
 --trace ("THUNK()" ++ showPpr (r, frees, self, isRec)) $ do

  genv <- askGlobalEnv

  -- 1. Create a new BCO for the body of the closure.
  info_tbl0 <- do
    let locs0 = mkLocs [ (arg, InReg n t) |
                         (arg, n) <- zip args [0..],
                         let t = repType (Ghc.varType arg) ]
        locs1 | isRec     = updateLoc locs0 self Self
              | otherwise = locs0
    let fvi = Ghc.mkVarEnv (zip frees [(1::Int)..])
    let env0 = mkLocalEnv (zip frees (repeat undefined))
  
    (is0, _locs, Nothing) <- withParentFun self $ transBody body env0 locs1 fvi RetC

    this_mdl <- getThisModule
    parent <- getParentFun
    let cl_prefix | Nothing <- parent = ".cl_"
                  | Just s  <- parent = ".cl_" ++ s ++ "_"

    x' <- freshVar (showPpr genv this_mdl ++ cl_prefix ++ Ghc.getOccString self) mkTopLevelId

    g <- finaliseBcGraph is0
    let arity = length args
        arg_types = map (transType genv . repType . Ghc.varType) args
        free_vars = M.fromList [ (n, transType genv (Ghc.varType v))
                                | (n, v) <- zip [1..] frees ]

    let bco = BcObject
              { bcoType = if arity > 0
                           then BcoFun arity arg_types
                           else if length frees > 0
                                then Thunk else CAF
              , bcoCode = g
              , bcoConstants = []
              , bcoGlobalRefs = [] -- TODO: What's this used for?
              , bcoFreeVars = free_vars }

    addBCO x' bco
    return x'

  if null frees then do
    let info_tbl = mkTopLevelId (idName info_tbl0)
    tag_reg <- mbFreshLocal (repType (Ghc.varType self)) Nothing
    return (insLoadGbl tag_reg info_tbl, locs0_outer, Left tag_reg,
            repType (Ghc.varType self), [])
   else do
     let info_tbl = mkInfoTableId (idName info_tbl0)
     (bcis1, locs1, regs) <- transArgs (map StgVarArg frees)
                                       env locs0_outer fvi_outer
     let tag_type = Ghc.bcoPrimTy
     tag_reg <- mbFreshLocal tag_type Nothing
     let fwds = [ FwdRef n x | (x, n) <- zip frees [1..]
                             , x `elem` fwds0 ]
     return (bcis1 <*> insLoadGbl tag_reg info_tbl, locs1,
             Right (tag_reg, regs), repType (Ghc.varType self), fwds)

type Fixup = [(BcVar, Int, BcVar)]

showSRT :: GlobalEnv -> SRT -> String
showSRT _env NoSRT                = "NoSRT"
showSRT env  (SRTEntries idset)   = showPpr env idset
showSRT _env (SRT ofs len bitmap) = "<bitmap>"

------------------------------------------------------------------------

transApp :: Ghc.Id -> [StgArg] -> LocalEnv -> KnownLocs
         -> FreeVarsIndex -> Context x
         -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transApp f args env locs0 fvi ctxt
  = do (is0, locs1, arg_regs) <- transArgs args env locs0 fvi
       (is1, f_reg, _, locs2) <- transVar f env fvi locs1 Nothing
       (is2, locs3, mb_r) <- transApp' f_reg arg_regs env locs2 fvi ctxt
       return (is0 <*> is1 <*> is2, locs3, mb_r)

transApp' :: BcVar -> [BcVar]
          -> LocalEnv -> KnownLocs
          -> FreeVarsIndex -> Context x
          -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transApp' f args env locs0 fvi ctxt
  | length args > cMAX_CALL_ARGS
  = do genv <- askGlobalEnv

       -- A call with more than cMAX_CALL_ARGS is translated as follows:
       --
       --     C[f x1 ... xN y1 ... yM]
       --
       --         =>
       --
       --     g <- call f x1 ... xN
       --     t1 <- call g y1 ... yM
       --     C[t1]
       --
       let (args1, args2) = splitAt cMAX_CALL_ARGS args
       let g_type = Ghc.anyTy
       (is0, locs1, Just g_reg) <- transApp' f args1 env locs0 fvi (BindC g_type Nothing)
       (is1, locs2, mb_rslt) <- transApp' g_reg args2 env locs1 fvi ctxt
       return (is0 <*> is1, locs2, mb_rslt)
       -- error $ "Call with too many args: " ++ showPpr genv f ++ " (" ++
       --         show (length args) ++ ", max: " ++ show cMAX_CALL_ARGS ++ ")\n" ++ 
       --         showPpr genv g_type

  | otherwise
  = do genv <- askGlobalEnv
       case ctxt of
         RetC -> -- tail call
           -- Ensure that tailcalls always use registers r0..r(N-1)
           -- for arguments.  This allows zero-copy function call.
           let typed_regs = [ BcReg n (transType genv (bcVarType r))
                            | (n,r) <- zip [0..] args ]
               is0 = catGraphs [ insMove tr r
                               | (tr,r) <- zip typed_regs args ]
               is1 = is0 <*> insCall Nothing f typed_regs
           in
           return (is1, locs0, Nothing)

         BindC rslt_ty0 opt_reg -> do
           -- need to ensure that x = O, so we need to emit
           -- a fresh label after the call
           let rslt_ty:_ = splitUnboxedTuples genv rslt_ty0
           r <- mbFreshLocal rslt_ty opt_reg
           let ins = withFresh $ \l ->
                       insCall (Just (r, l)) f args
                       |*><*| mkLabel l
           return (ins, locs0, Just r)

transArgs :: [StgArg] -> LocalEnv -> KnownLocs -> FreeVarsIndex
          -> Trans (Bcis O, KnownLocs, [BcVar])
transArgs args env locs0 fvi = go args emptyGraph locs0 []
 where
   go [] is locs regs = return (is, locs, reverse regs)
   go (StgVarArg x : xs) is locs regs = do
     (is', r, _, locs') <- transVar x env fvi locs Nothing
     go xs (is <*> is') locs' (r:regs)
   go (StgLitArg lit : xs) is locs regs = do
     (is', r) <- transLiteral lit Nothing
     go xs (is <*> is') locs (r:regs)

------------------------------------------------------------------------

isUnboxedTupleAlt :: AltType -> Bool
isUnboxedTupleAlt (UbxTupAlt _) = True
isUnboxedTupleAlt _ = False

bndrType :: Ghc.Id -> Ghc.Type
bndrType x = repType (Ghc.varType x)

transCase :: forall x.
             StgExpr -> Ghc.Id -> AltType -> [StgAlt]
          -> LocalEnv
          -> KnownLocs
          -> FreeVarsIndex
          -> Context x
          -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transCase expr bndr altty alts@[(altcon, vars, used, body)]
          env locs0 fvi ctxt
  | not (isUnboxedTupleAlt altty) = do
  -- Only one case alternative means we're just unwrapping
  (is0, locs1, Just r) <- transBody expr env locs0 fvi $!
                            BindC (bndrType bndr) Nothing
  let locs2 = updateLoc locs1 bndr (InVar r)
      env' = extendLocalEnv env bndr undefined
  let locs3 = addMatchLocs locs2 r altcon (zip vars used)
      env'' = extendLocalEnvList env vars
  (is1, locs4, mb_r) <- transBody body env'' locs3 fvi ctxt
  return (is0 <*> is1, locs4, mb_r)

transCase expr@(StgOpApp (StgPrimOp op) args alt_ty) bndr (AlgAlt tycon)
          alts env locs0 fvi ctxt
 | Just (cond, ty) <- isCondPrimOp op
 = case alts of
     [_,_] -> transBinaryCase cond ty args bndr alt_ty alts
                              env fvi locs0 ctxt
     [_] ->
       error "NYI: turn primop result into Bool"

transCase expr@(StgOpApp (StgPrimOp op) args alt_ty) bndr (PrimAlt tycon)
          alts env locs0 fvi ctxt
  | Just (cond, ty) <- isCondIntPrimOp op
  = case alts of
      [_,_] -> transBoolIntCase cond ty args bndr alt_ty alts
                                env fvi locs0 ctxt
      _ ->
        error "FATAL: unexpected case alternatives for primitive operation"

-- transCase expr@(StgOpApp (StgPrimOp op) args alt_ty) bndr (PrimAlt y)
--           alts env locs0 fvi ctxt
--  = assert (length alts >= 2) $ do
--   (is0, locs1, Just r) <- transBody expr env locs0 fvi $!
--                             BindC (bndrType bndr) Nothing
--   mb_dflt <- case alts of
--     (DEFAULT:other -> 
--   st <- getStack
--   error $ "NYI " ++ showPpr (expr, st)
 --       transBody build_bool_expr env fvi locs0 ctxt
 -- where
 --   build_bool_expr =
 --     Ghc.Case
 --       (Ghc.Case expr bndr Ghc.boolTy
 --          [(DataAlt Ghc.trueDataCon,  [], Ghc.mkConApp Ghc.trueDataCon [])
 --          ,(DataAlt Ghc.falseDataCon, [], Ghc.mkConApp Ghc.falseDataCon [])])
 --       bndr
 --       alt_ty
 --       alts

transCase expr bndr (AlgAlt tycon) alts env locs0 fvi ctxt = do
  -- Standard pattern matching on an algebraic datatype
  (is0, locs1, Just r) <- transBody expr env locs0 fvi $!
                            BindC (bndrType bndr) Nothing
  let locs2 = updateLoc locs1 bndr (InVar r)
      env' = extendLocalEnv env bndr undefined
  let tags = length (Ghc.tyConDataCons tycon)
  case ctxt of
    RetC -> do
      (alts, is2) <- transCaseAlts alts r env locs2 fvi RetC
      return ((is0 <*> insCase (CaseOnTag tags) {- XXX: wrong? -} r alts)
              `catGraphsC` is2, locs1, Nothing)
    BindC ty mr -> do
      -- We must be in a context:
      --
      --     case [[case ... of x1 { Ci -> ei; ... }]] of x2 { ... }
      --
      -- Note that x1 and x2 do not necessarily have the same type.  The return
      -- type of the case-alternatives `ei` is the same type as the type of
      -- `x1`.
      --
      -- The result of the case alternatives must be the type of the context
      -- (i.e., the type of x2).
      --
      r1 <- mbFreshLocal ty mr
      (alts, altIs) <- transCaseAlts alts r env locs2 fvi (BindC ty (Just r1))
      let is3 =
            withFresh $ \l ->
              let is' = [ i <*> insGoto l | i <- altIs ] in
              ((is0 <*> insCase (CaseOnTag tags) r alts) `catGraphsC` is')
                  |*><*| mkLabel l  -- make sure we're open at the end
      return (is3, locs1, Just r1)

transCase expr bndr (PrimAlt tycon) alts env0 locs0 fvi ctxt = do
  (bcis0, locs1, Just reg) <- transBody expr env0 locs0 fvi $!
                                BindC (bndrType bndr) Nothing

  -- bndr gets bound to the literal
  let locs2 = updateLoc locs1 bndr (InVar reg)
      env = extendLocalEnv env0 bndr undefined
  let (dflt, ty, tree) = buildCaseTree alts

  -- If the context requires binding to a variable, then we have to
  -- make sure all branches write their result into the same
  -- variable.
  ctxt' <- (case ctxt of
             RetC -> return RetC
             BindC ty mr ->
               -- let alt_ty = repType (Ghc.varType bndr) in
               BindC ty . Just <$> mbFreshLocal ty mr)
            :: Trans (Context x)

  end_label <- freshLabel

  let
    transArm :: StgExpr -> Trans (Label, BcGraph C C)
    transArm bdy = do
      l <- freshLabel
      (bcis, _locs', _mb_var) <- transBody bdy env locs2 fvi ctxt'
      case ctxt' of
        RetC -> 
          return (l, mkLabel l <*> bcis)
        BindC _ _ ->
          return (l, mkLabel l <*> bcis <*> insGoto end_label)

  (dflt_label, dflt_bcis) <- transArm dflt
  
  let
    build_branches :: CaseTree
                   -> Trans (Label, [BcGraph C C])

    build_branches (Leaf Nothing) = do
      return (dflt_label, [])
    build_branches (Leaf (Just expr)) = do
      (lbl, bci) <- transArm expr
      return (lbl, [bci])

    build_branches (Branch cmp lit true false) = do
      (true_lbl, true_bcis) <- build_branches true
      (false_lbl, false_bcis) <- build_branches false
      -- Ensure the code blocks are closed at the end
      (lit_bcis, lit_reg) <- transLiteral lit Nothing
      l <- freshLabel
      return (l, [mkLabel l <*> lit_bcis
                  <*> insBranch cmp ty reg lit_reg true_lbl false_lbl]
                 ++ true_bcis ++ false_bcis)

  case ctxt' of
    RetC -> do
      (l_root, bcis) <- build_branches tree
      return ((bcis0 <*> insGoto l_root) `catGraphsC` bcis
               |*><*| dflt_bcis,
              locs1, Nothing)
    BindC _ (Just r) -> do
      (l_root, bcis) <- build_branches tree
      return ((bcis0 <*> insGoto l_root) `catGraphsC` bcis
                |*><*| dflt_bcis |*><*| mkLabel end_label,
              locs1, Just r)

-- See [Note: Unarisation Weirdness] below
transCase (StgConApp dcon args) bndr (UbxTupAlt n) alts@[(_, vars, _, body)]
          env locs0 fvi ctxt
 | Ghc.isUnboxedTupleCon dcon
 = do
  -- args must be bound somewhere or be literals, so we just update
  -- the locations to match
  let match [] [] locs = locs
      match (StgVarArg arg:args') (var:vars') locs =
        let Just loc = lookupLoc locs arg in
        let locs' = updateLoc locs var loc in
        match args' vars' locs'
  let locs1 = match args vars locs0
  transBody body env locs1 fvi ctxt

transCase expr bndr (UbxTupAlt n) alts@[(_, vars, _, body)] env
          locs0 fvi ctxt = do

  (bcis, locs1, Just r0) <- transBody expr env locs0 fvi $!
                              BindC (bndrType bndr) Nothing

  genv <- askGlobalEnv

  -- Only non-void values are actually returned.
  let nonVoidVars = removeIf (isGhcVoid genv) vars
  case nonVoidVars of
    [var] -> do  -- same as regular return, really
      let locs2 = updateLoc locs1 var (InVar r0)
          env' = extendLocalEnv env var undefined
      (bcis', locs3, mb_r) <- transBody body env' locs2 fvi ctxt
      return (bcis <*> bcis', locs3, mb_r)

    resultVar0:(otherResultVars@(_:_)) -> do
      -- Leave `bndr` undefined.  It should always be a wildcard.
      let env' = extendLocalEnvList env nonVoidVars

      -- Result variables don't survive across multiple CALL instructions
      -- so we load them all into fresh variables.
      regs <- mapM (\x -> mbFreshLocal (repType (Ghc.varType x)) Nothing)
                   otherResultVars
      let bcis1 = [ insLoadExtraResult r n | (r, n) <- zip regs [1..] ]
      let locs2 = extendLocs locs1 [(resultVar0, InVar r0)]
      let locs3 = extendLocs locs2
                    [ (x, InVar r) | (x, r) <- zip otherResultVars regs ]

      (bcis', locs4, mb_r) <- transBody body env' locs3 fvi ctxt
      return (bcis <*> catGraphs bcis1 <*> bcis', locs4, mb_r)

  -- error "NYI: Case on unboxed tuple"

transCase expr bndr alt_ty alts env locs0 fvi ctxt = do
  genv <- askGlobalEnv
  error $ "NYI: Case expression: " ++ showPpr genv (alt_ty, expr, alts)

------------------------------------------------------------------------
{-
* Note: Unarisation Weirdness

The following program gives rise to some weird STG which doesn't actually
pass the StgLint pass:

    d x = case e x of
      (# y, z #) -> y +# z

    {-# NOINLINE e #-}
    e :: Int# -> (# Int#, Int# #)
    e x = (# x, x #)

Is translated into STG as follows:

    c.Bc0001.e [InlPrag=NOINLINE]
      :: GHC.Prim.Int# -> (# GHC.Prim.Int#, GHC.Prim.Int# #)
    [GblId, Arity=1, Caf=NoCafRefs, Unf=OtherCon []] =
        \r [x_sP] (#,#) [x_sP x_sP];

    SRT(Bc.Bc0001.e): []
    Bc.Bc0001.d :: GHC.Prim.Int# -> GHC.Prim.Int#
    [GblId, Arity=1, Caf=NoCafRefs, Unf=OtherCon []] =
        \r [x_sR]
            case Bc.Bc0001.e x_sR of ds_sV {
              (#,#) ipv_s13 ipv1_s14 ->
                  case (#,#) [ipv_s13 ipv1_s14] of _ {    --- (!)
                    (#,#) y_sZ z_s10 -> +# [y_sZ z_s10];  --- (!)
                  };
            };
    SRT(Bc.Bc0001.d): []

This lines annotated with (!) are clearly unnecessary.  We thus
use a custom hack and perform this known-case transformation
during the translation to bytecode.

-}
------------------------------------------------------------------------

transCaseAlts :: [StgAlt] -> BcVar -> LocalEnv
              -> KnownLocs -> FreeVarsIndex -> Context x
              -> Trans ([(BcTag, BlockId)], [BcGraph C x])
transCaseAlts alts match_var env locs0 fvi ctxt = do
  (targets, bcis) <- unzip <$>
    (forM alts $ \(altcon, vars, used, body) -> do
      let locs1 = addMatchLocs locs0 match_var altcon (zip vars used)
          env' = extendLocalEnvList env vars
      (bcis, _locs2, _mb_var) <- transBody body env' locs1 fvi ctxt
      l <- freshLabel
      return ((dataConTag altcon, l), mkLabel l <*> bcis))
  return (targets, bcis)

------------------------------------------------------------------------

data CaseTree
  = Leaf (Maybe StgExpr)  -- execute this code (or default)
  | Branch CmpOp Ghc.Literal CaseTree CaseTree
    -- cmp + ty,  true_case, false_case

-- | Given a list of literal pattern matches, builds a balanced tree.
--
-- The goal is for this tree to select among the @N@ alternatives in
-- @log2(N)@ time.
--
-- TODO: Detect and take advantage of ranges.
buildCaseTree :: [StgAlt]
              -> (StgExpr, OpTy, CaseTree)
                 -- ^ Default code, comparison type, and other cases
buildCaseTree ((DEFAULT, [], _, dflt_expr):alts0) =
  assert alts_is_sorted $ (dflt_expr, ty, buildTree alts)
 where
   alts = map simpl_lit alts0

   alts_is_sorted =
     map fst (sortBy (comparing fst) alts) == map fst alts

   dflt = Leaf Nothing
   leaf x = Leaf (Just x)

   simpl_lit (LitAlt lit, [], [], expr) =
     assert (ghcLiteralType lit == ty) $ (lit, expr)

   ty = case alts0 of ((LitAlt l, _, _, _):_) -> ghcLiteralType l

   buildTree [(l, body)] =
     Branch CmpEq l (leaf body) dflt
   buildTree [(l1, body1), (l2,body2)] =
     Branch CmpEq l1 (leaf body1) (Branch CmpEq l2 (leaf body2) dflt)
   buildTree alts1 =
     let l = length alts1 in
     case splitAt (l `div` 2) alts1 of
       (lows, highs@((l, _):_)) ->
         Branch CmpGe l (buildTree highs) (buildTree lows)

------------------------------------------------------------------------

type Used = Bool

addMatchLocs :: KnownLocs -> BcVar -> AltCon -> [(Ghc.Id, Used)] -> KnownLocs
addMatchLocs locs _base_reg DEFAULT [] = locs
addMatchLocs locs _base_reg (LitAlt _) [] = locs
addMatchLocs locs base_reg (DataAlt _) vars =
  extendLocs locs [ (x, Field base_reg n t)
                  | ((x,_used), n) <- zip vars [1..]
                  , let t = repType (Ghc.varType x) ]

dataConTag :: AltCon -> BcTag
dataConTag DEFAULT = DefaultTag
dataConTag (DataAlt dcon) = Tag $ Ghc.dataConTag dcon
dataConTag (LitAlt (Ghc.MachInt n)) = LitT n

------------------------------------------------------------------------

-- | Translate a binary case on a primop, i.e., a two-arm branch
-- with alternatives @True@ or @False@.
transBinaryCase :: forall x.
                   CmpOp -> OpTy -> [StgArg] -> CoreBndr
                -> Ghc.Type -> [StgAlt]
                -> LocalEnv -> FreeVarsIndex
                -> KnownLocs -> Context x
                -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transBinaryCase cond ty args bndr alt_ty alts@[_,_] env0 fvi locs0 ctxt = do
  -- TODO: We may want to get the result of the comparison as a
  -- Bool.  In the True branch we therefore want to have:
  --
  -- > bndr :-> loadLit True
  --
  genv <- askGlobalEnv
  (bcis, locs1, [r1, r2]) <- transArgs args env0 locs0 fvi
--  let locs2 = updateLoc locs1 bndr (InVar r)
  let env = extendLocalEnv env0 bndr undefined
  -- let match_var = error "There must be no binders in comparison binops"
  let (trueBody, falseBody) =
        case alts of
          [(DEFAULT, [], _, b1), (DataAlt c, [], _, b2)]
           | c == Ghc.trueDataCon  -> (b2, b1)
           | c == Ghc.falseDataCon -> (b1, b2)
          [(DataAlt c1, [], _, b1), (DataAlt _, [], _, b2)]
           | c1 == Ghc.trueDataCon  -> (b1, b2)
           | c1 == Ghc.falseDataCon -> (b2, b1)
  -- If the context requires binding to a variable, then we have to
  -- make sure both branches write their result into the same
  -- variable.
  ctxt' <- (case ctxt of
             RetC -> return RetC
             BindC ty mr -> BindC ty . Just <$> mbFreshLocal ty mr)
            :: Trans (Context x)

  let transUnaryConAlt body dcon con_id = do
        let locs2 = updateLoc locs1 bndr (Global (dataConVarType dcon) con_id)
        l <- freshLabel
        (bcis, _locs1, _mb_var)
          <- transBody body env locs2 fvi ctxt'
        return (l, mkLabel l <*> bcis)

  (tLabel, tBcis) <- transUnaryConAlt trueBody Ghc.trueDataCon (trueDataConId genv)
  (fLabel, fBcis) <- transUnaryConAlt falseBody Ghc.falseDataCon (falseDataConId genv)

  case ctxt' of
    RetC -> do
      return (bcis <*> insBranch cond ty r1 r2 tLabel fLabel
                   |*><*| tBcis |*><*| fBcis,
              locs1, Nothing)
    BindC _ (Just r) -> do
      l <- freshLabel
      return (bcis <*> insBranch cond ty r1 r2 tLabel fLabel
                |*><*| tBcis <*> insGoto l
                |*><*| fBcis <*> insGoto l
                |*><*| mkLabel l,
              locs1, Just r)

-- | Translate a binary case on a primop which returns 0 or 1.
transBoolIntCase :: forall x.
                    CmpOp -> OpTy -> [StgArg] -> CoreBndr
                 -> Ghc.Type -> [StgAlt]
                 -> LocalEnv -> FreeVarsIndex
                 -> KnownLocs -> Context x
                 -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transBoolIntCase cond ty args bndr alt_ty alts@[_,_] env0 fvi locs0 ctxt = do

  genv <- askGlobalEnv
  (is0, locs1, [r1, r2]) <- transArgs args env0 locs0 fvi
  let env1 = extendLocalEnv env0 bndr undefined
  -- Normalise case alternatives
  let (trueBody, falseBody) =
        case alts of
          [(DEFAULT, [], _, body1), (LitAlt (Ghc.MachInt n), [], _, body2)]
           | n == 1 -> (body2, body1)
           | n == 0 -> (body1, body2)
          [(LitAlt (Ghc.MachInt n1), [], _, body1),
           (LitAlt (Ghc.MachInt n2), [], _, body2)]
           | n1 == 0, n2 == 1 -> (body2, body1)
           | n1 == 1, n2 == 0 -> (body1, body2)
          _ -> error $ "Unexpected int-boolean alternatives" <>
                       showPpr genv alts

  ctxt' <- (case ctxt of
              RetC -> return RetC
              BindC ty mr -> BindC ty . Just <$> mbFreshLocal ty mr)
             :: Trans (Context x)

  let transUnaryConAlt body n = do
        let locs2 = updateLoc locs1 bndr (Literal Ghc.intPrimTy (CInt n))
        l <- freshLabel
        (bcis, _locs1, _mb_var) <- transBody body env1 locs1 fvi ctxt'
        return (l, mkLabel l <*> bcis)

  (tLabel, t_is) <- transUnaryConAlt trueBody 1
  (fLabel, f_is) <- transUnaryConAlt falseBody 0

  case ctxt' of
    RetC ->
      return (is0 <*> insBranch cond ty r1 r2 tLabel fLabel
                |*><*| t_is |*><*| f_is,
              locs1, Nothing)
    BindC _ (Just r) -> do
      l <- freshLabel
      return (is0 <*> insBranch cond ty r1 r2 tLabel fLabel
                |*><*| t_is <*> insGoto l
                |*><*| f_is <*> insGoto l
                |*><*| mkLabel l,
              locs1, Just r)

------------------------------------------------------------------------

-- | Translate a variable reference into bytecode.
--
-- Ensures that the variable is loaded into a register.  Uses
-- 'KnownLocs' to figure out the existing location (if any).
--
-- If the variable already has already been loaded into another
-- variable this creates a move instruction (which may later be
-- removed by the register allocator).
--
-- INVARIANT: @Closure-vars = dom(LocalEnv) - dom(KnownLocs)@
--
transVar ::
     CoreBndr
  -> LocalEnv
  -> FreeVarsIndex
  -> KnownLocs
  -> Maybe BcVar -- ^ @Just r <=>@ load variable into specified
                 -- register.
  -> Trans (Bcis O, BcVar, Bool, KnownLocs)
     -- ^ Returns:
     --
     -- * The instructions to load the variable
     --
     -- * The register it has been loaded into
     --
     -- * @True <=>@ the variable is known to be in WHNF (e.g., a
     -- top-level function).
     --
     -- * Updated 'KnownLocs'
     --

-- transVar x _ _ _ _ | trace ("transVar: " ++ showPpr x ++ " : "
--                            ++ showPpr (Ghc.idType x) ++ " / "
--                            ++ show (not (Ghc.isUnLiftedType (Ghc.idType x))))
--                     False = undefined
transVar x env fvi locs0 mr = do
  genv <- askGlobalEnv
  case lookupLoc locs0 x of
    Just (InVar x') -> {- trace ("inVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ do -}
      return (mbMove mr x', fromMaybe x' mr, in_whnf, locs0)
    Just (InReg r ty) -> do -- trace "inREG" $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType genv ty)), x', in_whnf,
              updateLoc locs0 x (InVar x'))
    Just (Field p n ty) -> do -- trace "inFLD" $ do
      --trace ("Field:" ++ show (p,bcVarType p,n) ++ ":" ++ ghcPretty ty) $ do
      r <- mbFreshLocal ty mr
      return (insFetch r p n,
              r, in_whnf, updateLoc locs0 x (InVar r))
    Just Fwd -> do
      -- A forward reference is replaced by a black hole and which
      -- does not need to be followed by anything.  So we give it a
      -- silly type.
      r <- mbFreshLocal Ghc.unitTy mr
      return (insLoadBlackhole r, r, True, locs0)
    Just Self -> do
      -- TODO: Find out the real type of Self closure?  It's always a
      -- pointer so (Any :: *) should be fine for now.
      r <- mbFreshLocal ghcAnyType mr
      return (insLoadSelf r, r, True, locs0)
    Just Void -> do
      r <- mbFreshLocal Ghc.realWorldStatePrimTy mr
      return (emptyGraph, r, True, locs0)
    Just (Global ty g) -> do
      r <- mbFreshLocal ty mr
      return (insLoadGbl r g, r, True, updateLoc locs0 x (InVar r))
    Just (Literal ty c) -> do
      r <- mbFreshLocal ty mr
      return (insLoadLit r c, r, True, updateLoc locs0 x (InVar r))
    Nothing
      | Just x' <- lookupLocalEnv env x -> do
          -- Note: To avoid keeping track of two environments we must
          -- only reach this case if the variable is bound outside the
          -- current closure.
          if isGhcVoid genv x then
            error "Free variables of type void not yet supported."
           else do
            r <- mbFreshLocal (repType (Ghc.varType x)) mr
            -- Do not force @i@ -- must remain a thunk
            let i = expectJust "transVar" (Ghc.lookupVarEnv fvi x)
            return (insLoadFV r i, r, in_whnf,
                    updateLoc locs0 x (InVar r))

      | otherwise -> do  -- global variable
          this_mdl <- getThisModule
          let x' = toplevelId genv this_mdl x
          r <- mbFreshLocal (repType (Ghc.varType x)) mr
          {- trace ("VARadd:" ++ ppVar x ++ " : " ++ pretty x') $ do -}
          return (insLoadGbl r x', r, isGhcConWorkId x,  -- TODO: only if CAF
                  updateLoc locs0 x (InVar r))
    r -> error $ "transVar: unhandled case: " ++ pretty genv r ++ " "
              ++ showPpr genv x
 where
   in_whnf = Ghc.isUnLiftedType (Ghc.idType x)

------------------------------------------------------------------------

dataConVarType :: DataCon -> Ghc.Type
dataConVarType dcon
  | Ghc.isNullarySrcDataCon dcon = ghcAnyType
  | otherwise                    = Ghc.bcoPrimTy
    -- We use BCO# as the type of "pointers to things that the GC doesn't have
    -- to traverse" (yes, it's hacky)

loadDataCon :: DataCon -> LocalEnv -> FreeVarsIndex -> KnownLocs
            -> Maybe BcVar
            -> Trans (Bcis O, KnownLocs, BcVar)
loadDataCon dcon env fvi locs0 mr = do
  genv <- askGlobalEnv
  let x = dataConWorkId dcon
  case lookupItblLoc locs0 x of
    Just (InVar x') ->
      {- trace ("DCONinVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ -}
      return (mbMove mr x', locs0, fromMaybe x' mr)
    Just (InReg r ty) -> do
      -- trace ("DCONinREG (" ++ pretty r ++ ") <> " ++ ppVar  x) $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType genv ty)),
              updateItblLoc locs0 x (InVar x'), x')
    Nothing -> do
      this_mdl <- getThisModule
      let is_nullary = Ghc.isNullarySrcDataCon dcon
      let x' | is_nullary = toplevelId genv this_mdl x
             | otherwise  = dataConInfoTableId genv dcon
      let ty = dataConVarType dcon
      -- trace ("DCONadd: " ++ ppVar x ++ " : " ++ pretty x') $ do
      r <- mbFreshLocal ty mr
      return (insLoadGbl r x',
              updateItblLoc locs0 x (InVar r),
              r)-- TODO: only if CAF

------------------------------------------------------------------------

transStore :: DataCon -> [StgArg] -> LocalEnv -> KnownLocs -> FreeVarsIndex
           -> Context x
           -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transStore dcon args env locs0 fvi ctxt = do
  (is0, locs1, regs) <- transArgs args env locs0 fvi
  (is1, locs2, rdcon) <- loadDataCon dcon env fvi locs1 (contextVar ctxt)
  let rslt_ty = dataConOrigResTy dcon
  rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
  let is = (is0 <*> is1) <*> insAlloc rslt rdcon regs
  maybeAddRet ctxt is locs2 rslt

------------------------------------------------------------------------

-- | Append a @Ret1@ instruction if needed and return.
maybeAddRet :: Context x -> Bcis O -> KnownLocs -> BcVar
            -> Trans (Bcis x, KnownLocs, Maybe BcVar)
maybeAddRet (BindC _ _) is locs r =
  return (is, locs, Just r)
maybeAddRet RetC is locs r =
  return (is <*> insRet1 r, locs, Nothing)

------------------------------------------------------------------------

repType :: a -> a
repType = id

-- | Return the (GHC) type of the given variable which must not be a
-- register.
bcVarType :: BcVar -> Ghc.Type
bcVarType (BcVar _ t) = t
bcVarType (BcReg _ _) = error "bcVarType: Not a variable but a register"

------------------------------------------------------------------------

-- | Translate a literal into bytecode.
--
-- Usually just amounts to loading a value from the constant pool.
-- Indices of the constant pool are determined in a separate pass.
transLiteral :: Ghc.Literal -> Maybe BcVar
             -> Trans (Bcis O, BcVar)
transLiteral lit mbvar = do
  rslt <- mbFreshLocal (Ghc.literalType lit) mbvar
  return (insLoadLit rslt (fromGhcLiteral lit), rslt)

fromGhcLiteral :: Ghc.Literal -> BcConst
fromGhcLiteral lit = case lit of
#if __GLASGOW_HASKELL__ <= 706
  Ghc.MachStr fs   -> CStr (unpackFS fs)
#elif __GLASGOW_HASKELL__ >= 707 && __GLASGOW_HASKELL__ < 709
  Ghc.MachStr bs   -> CStr (BC8.unpack bs)  -- UTF8 encoded
#endif
  Ghc.MachChar c   -> CChar c
  Ghc.MachInt n    -> CInt n
  Ghc.MachInt64 n  -> CInt64 n
  Ghc.MachWord n   -> CWord n
  Ghc.MachWord64 n -> CWord64 n
  Ghc.MachFloat r  -> CFloat r
  Ghc.MachDouble r -> CDouble r

------------------------------------------------------------------------

-- | Return a move instruction if target is @Just x@.
--
-- Redundant move instructions are eliminated by a later pass.
mbMove :: Maybe BcVar -> BcVar -> Bcis O
mbMove Nothing _ = emptyGraph
mbMove (Just r) r'
  | r == r'   = emptyGraph
  | otherwise = insMove r r'

isGhcConWorkId :: CoreBndr -> Bool
isGhcConWorkId x
  | Ghc.DataConWorkId _ <- Ghc.idDetails x = True
  | otherwise                              = False

ghcIdDataCon :: CoreBndr -> Ghc.DataCon
ghcIdDataCon x
  | Ghc.DataConWorkId dcon <- Ghc.idDetails x = dcon
  | otherwise = error "ghcIdDataCon: Id is not a DataConWorkId"

------------------------------------------------------------------------

-- TODO: This needs more thought.
primOpToBinOp :: Ghc.PrimOp -> Maybe (BinOp, OpTy)
primOpToBinOp primop =
  case primop of
    Ghc.IntAddOp -> Just (OpAdd, IntTy)
    Ghc.IntSubOp -> Just (OpSub, IntTy)
    Ghc.IntMulOp -> Just (OpMul, IntTy)
    Ghc.IntQuotOp -> Just (OpDiv, IntTy)
    Ghc.IntRemOp  -> Just (OpRem, IntTy)

    Ghc.WordAddOp -> Just (OpAdd, WordTy)
    Ghc.WordSubOp -> Just (OpSub, WordTy)
    Ghc.WordMulOp -> Just (OpMul, WordTy)

    Ghc.IntGtOp -> Just (CmpGtI, IntTy)
    Ghc.IntGeOp -> Just (CmpGeI, IntTy)
    Ghc.IntEqOp -> Just (CmpEqI, IntTy)
    Ghc.IntNeOp -> Just (CmpNeI, IntTy)
    Ghc.IntLtOp -> Just (CmpLtI, IntTy)
    Ghc.IntLeOp -> Just (CmpLeI, IntTy)

    Ghc.CharGtOp -> Just (CmpGtI, CharTy)
    Ghc.CharGeOp -> Just (CmpGeI, CharTy)
    Ghc.CharEqOp -> Just (CmpEqI, CharTy)
    Ghc.CharNeOp -> Just (CmpNeI, CharTy)
    Ghc.CharLtOp -> Just (CmpLtI, CharTy)
    Ghc.CharLeOp -> Just (CmpLeI, CharTy)

    Ghc.WordGtOp -> Just (CmpGtI, WordTy)
    Ghc.WordGeOp -> Just (CmpGeI, WordTy)
    Ghc.WordEqOp -> Just (CmpEqI, WordTy)
    Ghc.WordNeOp -> Just (CmpNeI, WordTy)
    Ghc.WordLtOp -> Just (CmpLtI, WordTy)
    Ghc.WordLeOp -> Just (CmpLeI, WordTy)

    _ -> Nothing

isCondPrimOp :: Ghc.PrimOp -> Maybe (CmpOp, OpTy)
isCondPrimOp primop =
  case primop of
    _ -> Nothing

-- | Is the given prim-op a "boolean" prim-op that returns @1@ if true and @0@
-- if false?  Examples: @eqWord# :: Word# -> Word# -> Int#@
isCondIntPrimOp :: Ghc.PrimOp -> Maybe (CmpOp, OpTy)
isCondIntPrimOp primop =
  case primop of
    Ghc.IntGtOp -> Just (CmpGt, IntTy)
    Ghc.IntGeOp -> Just (CmpGe, IntTy)
    Ghc.IntEqOp -> Just (CmpEq, IntTy)
    Ghc.IntNeOp -> Just (CmpNe, IntTy)
    Ghc.IntLtOp -> Just (CmpLt, IntTy)
    Ghc.IntLeOp -> Just (CmpLe, IntTy)

    Ghc.CharGtOp -> Just (CmpGt, CharTy)
    Ghc.CharGeOp -> Just (CmpGe, CharTy)
    Ghc.CharEqOp -> Just (CmpEq, CharTy)
    Ghc.CharNeOp -> Just (CmpNe, CharTy)
    Ghc.CharLtOp -> Just (CmpLt, CharTy)
    Ghc.CharLeOp -> Just (CmpLe, CharTy)

    Ghc.WordGtOp -> Just (CmpGt, WordTy)
    Ghc.WordGeOp -> Just (CmpGe, WordTy)
    Ghc.WordEqOp -> Just (CmpEq, WordTy)
    Ghc.WordNeOp -> Just (CmpNe, WordTy)
    Ghc.WordLtOp -> Just (CmpLt, WordTy)
    Ghc.WordLeOp -> Just (CmpLe, WordTy)

    _ -> Nothing


primOpOther :: Ghc.PrimOp -> Maybe (PrimOp, [OpTy], OpTy)
primOpOther primop =
  case primop of
{-
    Ghc.IntGtOp -> Just (CmpGtI, [IntTy, IntTy], IntTy)
    Ghc.IntGeOp -> Just (CmpGeI, [IntTy, IntTy], IntTy)
    Ghc.IntEqOp -> Just (CmpEqI, [IntTy, IntTy], IntTy)
    Ghc.IntNeOp -> Just (CmpNeI, [IntTy, IntTy], IntTy)
    Ghc.IntLtOp -> Just (CmpLtI, [IntTy, IntTy], IntTy)
    Ghc.IntLeOp -> Just (CmpLeI, [IntTy, IntTy], IntTy)

    Ghc.CharGtOp -> Just (CmpGtI, [CharTy, CharTy], IntTy)
    Ghc.CharGeOp -> Just (CmpGeI, [CharTy, CharTy], IntTy)
    Ghc.CharEqOp -> Just (CmpEqI, [CharTy, CharTy], IntTy)
    Ghc.CharNeOp -> Just (CmpNeI, [CharTy, CharTy], IntTy)
    Ghc.CharLtOp -> Just (CmpLtI, [CharTy, CharTy], IntTy)
    Ghc.CharLeOp -> Just (CmpLeI, [CharTy, CharTy], IntTy)

    Ghc.WordGtOp -> Just (CmpGtI, [WordTy, WordTy], IntTy)
    Ghc.WordGeOp -> Just (CmpGeI, [WordTy, WordTy], IntTy)
    Ghc.WordEqOp -> Just (CmpEqI, [WordTy, WordTy], IntTy)
    Ghc.WordNeOp -> Just (CmpNeI, [WordTy, WordTy], IntTy)
    Ghc.WordLtOp -> Just (CmpLtI, [WordTy, WordTy], IntTy)
    Ghc.WordLeOp -> Just (CmpLeI, [WordTy, WordTy], IntTy)
-}
    Ghc.IndexOffAddrOp_Char -> Just (OpIndexOffAddrChar, [AddrTy, IntTy], CharTy)
    Ghc.DataToTagOp -> Just (OpGetTag, [PtrTy], IntTy)
    Ghc.IntNegOp -> Just (OpNegateInt, [IntTy], IntTy)

    Ghc.AndOp -> Just (OpBitAnd, [WordTy, WordTy], WordTy)
    Ghc.OrOp  -> Just (OpBitOr,  [WordTy, WordTy], WordTy)
    Ghc.XorOp -> Just (OpBitXor, [WordTy, WordTy], WordTy)
    Ghc.NotOp -> Just (OpBitNot, [WordTy], WordTy)

    -- bit shifting
    Ghc.SllOp -> Just (OpShiftLeft, [WordTy, IntTy], WordTy)
    Ghc.SrlOp -> Just (OpShiftRightLogical, [WordTy, IntTy], WordTy)
    Ghc.ISllOp -> Just (OpShiftLeft, [IntTy, IntTy], IntTy)
    Ghc.ISraOp -> Just (OpShiftRightArith, [IntTy, IntTy], IntTy)
    Ghc.ISrlOp -> Just (OpShiftRightLogical, [IntTy, IntTy], IntTy)

    -- these are all NOPs
    Ghc.OrdOp -> Just (OpNop, [CharTy], IntTy)
    Ghc.ChrOp -> Just (OpNop, [IntTy], CharTy)
    Ghc.Addr2IntOp -> Just (OpNop, [AddrTy], IntTy)
    Ghc.Int2AddrOp -> Just (OpNop, [IntTy], AddrTy)
    Ghc.Word2IntOp -> Just (OpNop, [WordTy], IntTy)
    Ghc.Int2WordOp -> Just (OpNop, [IntTy], WordTy)
    Ghc.RaiseOp -> Just (OpRaise, [PtrTy], VoidTy)

    _ -> Nothing

ghcLiteralType :: Ghc.Literal -> OpTy
ghcLiteralType lit = case lit of
  Ghc.MachInt _    -> IntTy
  Ghc.MachInt64 _  -> Int64Ty
  Ghc.MachChar _   -> CharTy
  Ghc.MachWord _   -> WordTy
  Ghc.MachWord64 _ -> Word64Ty
  Ghc.MachStr _    -> AddrTy
  Ghc.MachFloat _  -> FloatTy
  Ghc.MachDouble _ -> DoubleTy
