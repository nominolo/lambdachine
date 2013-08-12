{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
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
import DataCon ( DataCon, dataConWorkId, dataConRepType )
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
import DynFlags ( tracingDynFlags )
import qualified Outputable as Out
import qualified Pretty as Out
import CoreSyn ( CoreBndr )
import StgSyn
import Var ( isTyVar )
import Unique ( Uniquable(..), getKey )
import FastString ( unpackFS )

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

tracePpr :: Outputable a => a -> b -> b
tracePpr o exp = trace (">>> " ++ showPpr o) exp

showPpr1 :: Outputable a => a -> String
showPpr1 o = Out.showDocWith Out.OneLineMode $
  Out.withPprStyleDoc tracingDynFlags Out.defaultUserStyle (Out.ppr o)

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

stgToBytecode :: Supply Unique
              -> Ghc.ModuleName
              -> [StgBinding]
              -> [TyCon]
              -> BCOs
stgToBytecode uniques mdl bndrs0 data_tycons =
  runTrans mdl uniques $ do
    mapM_ transTopLevelBind bndrs0
    getBCOs

------------------------------------------------------------------------
-- The bytecode translation generation monad

newtype Trans a = Trans { unTrans :: State TransState a }
  deriving (Functor, Applicative, Monad, MonadFix)

data TransState = TransState
  { tsUniques    :: Supply Unique
  , tsLocalBCOs  :: BCOs
  , tsModuleName :: !Ghc.ModuleName
  , tsParentFun  :: Maybe String
  }

runTrans :: Ghc.ModuleName -> Supply Unique -> Trans a -> a
runTrans mdl us (Trans m) = evalState m s0
 where
   s0 = TransState { tsUniques = us
                   , tsLocalBCOs = M.empty
                   , tsModuleName = mdl
                   , tsParentFun = Nothing }

genUnique :: Trans (Supply Unique)
genUnique = Trans $ do
  s <- get
  case split2 (tsUniques s) of
    (us, us') -> do
      put $! s{ tsUniques = us }
      return us'

instance UniqueMonad Trans where
  freshUnique = hooplUniqueFromUniqueSupply `fmap` genUnique

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
withParentFun x (Trans act) = Trans $ do
  s <- get
  let x_occ = showSDocForUser Ghc.neverQualify (Ghc.ppr (Ghc.getOccName x))
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
  | Global Id
    -- ^ The value is a top-level ID.
  | Void
    -- ^ The value does not have a representation.
  deriving Show

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
  x' <- (`toplevelId` x) <$> getThisModule
  transTopLevelRhs x x' rhs

transTopLevelRhs :: Ghc.Id -> Id -> StgRhs -> Trans ()
transTopLevelRhs bndr0 bndr rhs =
  case rhs of
    StgRhsCon _ccs dcon args ->
      transTopLevelRhsConstr bndr dcon args

    StgRhsClosure _ccs bndrInfo frees upd _srt args body -> do
      invariant ("Toplevel closure cannot have any free variables" ++
                 showPpr rhs)$
        null frees
      transTopLevelRhsClosure bndr0 bndr bndrInfo upd args body

------------------------------------------------------------------------

transTopLevelRhsConstr :: Id -> DataCon -> [StgArg] -> Trans ()
transTopLevelRhsConstr x dcon args = do
  invariant ("Datacon arity must match given no. of args: " ++
             showPpr1 (dcon, args)) $
    args `lengthIs` Ghc.dataConRepRepArity dcon

  this_mdl <- getThisModule
  let dcon' = dataConInfoTableId dcon
      fields = transFields (toplevelId this_mdl) args
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
  let bco_type
        | (_:_) <- args = BcoFun (length args) (map (transType . {- Ghc.repType .-}  Ghc.varType) args)
        | otherwise     = CAF

  let env0 = mkLocalEnv [(x, undefined) | x <- args]
      fvi0 = Ghc.emptyVarEnv
      locs0 = mkLocs [ (b, InReg n t)
                     | (b, n) <- zip args [0..]
                     , let t = repType (Ghc.varType b) ]

  (bcis, locs1, Nothing) <- withParentFun x0 $ transBody body env0 locs0 fvi0 RetC


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
  BindC :: Maybe BcVar -> Context O

contextVar :: Context x -> Maybe BcVar
contextVar RetC = Nothing
contextVar (BindC mx) = mx

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
    BindC _ -> return (is, locs0, Just r)

transBody (StgApp x []) env locs0 fvi ctxt = do
  (is0, r, eval'd, locs1) <- transVar x env fvi locs0 (contextVar ctxt)
  let is | eval'd = is0
         | otherwise = withFresh $ \l ->
                         is0 <*> insEval l r |*><*| mkLabel l
  case ctxt of
    RetC -> return (is <*> insRet1 r, locs1, Nothing)
    BindC _ -> return (is, locs1, Just r)

transBody (StgApp f args) env locs0 fvi ctxt =
  transApp f args env locs0 fvi ctxt

transBody (StgConApp dcon []) env locs0 fvi ctxt = do
  let dcon_closure = dataConWorkId dcon
  (is, r, _, locs1) <- transVar dcon_closure env fvi locs0 (contextVar ctxt)
  maybeAddRet ctxt is locs1 r

transBody (StgConApp dcon args) env locs0 fvi ctxt = do
  (is0, locs1, regs) <- transArgs args env locs0 fvi
  (is1, locs2, con_reg) <- loadDataCon dcon env fvi locs1 (contextVar ctxt)
  let Just (arg_tys, rslt_ty) =
        splitFunTysN (length args) $ dataConRepType dcon
  rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
  let is2 = (is0 <*> is1) <*> insAlloc rslt con_reg regs
  maybeAddRet ctxt is2 locs2 rslt

--transBody _ env ctxt = return (emptyGraph, Nothing)

------------------------------------------------------------------------

transApp :: Ghc.Id -> [StgArg] -> LocalEnv -> KnownLocs
         -> FreeVarsIndex -> Context x
         -> Trans (Bcis x, KnownLocs, Maybe BcVar)
transApp f args env locs0 fvi ctxt
  | length args > cMAX_CALL_ARGS
  = error $ "Call with too many args: " ++ showPpr f ++ " (" ++
            show (length args) ++ ")"
  | otherwise
  = do (is0, locs1, regs) <- transArgs args env locs0 fvi
       (is1, fr, _, locs2) <- transVar f env fvi locs1 Nothing
       let is2 = is0 <*> is1
       case ctxt of
         RetC -> -- tailcal
           -- Ensure that tailcalls always use registers r0..r(N-1)
           -- for arguments.  This allows zero-copy function call.
           let typed_regs = [ BcReg n (transType (bcVarType r))
                            | (n,r) <- zip [0..] regs ]
               is3 = is2 <*>
                      catGraphs [ insMove tr r
                                | (tr,r) <- zip typed_regs regs ]
               is4 = is3 <*> insCall Nothing fr typed_regs
           in
           return (is4, locs2, Nothing)

         BindC opt_reg -> do
           -- need to ensure that x = O, so we need to emit
           -- a fresh label after the call
           let rslt_ty0 =
                 case splitFunTysN (length args) $
                        repType (Ghc.varType f) of
                   Just (_arg_tys, rslt_ty_) -> rslt_ty_
                   Nothing -> error $ "Result type for: " ++
                                ghcPretty (f, repType (Ghc.varType f),
                                           Ghc.varType f, length args)
               rslt_ty:_ = splitUnboxedTuples rslt_ty0
           r <- mbFreshLocal rslt_ty opt_reg
           let ins = withFresh $ \l ->
                       is2 <*> insCall (Just (r, l)) fr regs
                       |*><*| mkLabel l
           return (ins, locs2, Just r)

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
transVar x env fvi locs0 mr =
  case lookupLoc locs0 x of
    Just (InVar x') -> {- trace ("inVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ do -}
      return (mbMove mr x', fromMaybe x' mr, in_whnf, locs0)
    Just (InReg r ty) -> do -- trace "inREG" $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType ty)), x', in_whnf,
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
    Nothing
      | Just x' <- lookupLocalEnv env x -> do
          -- Note: To avoid keeping track of two environments we must
          -- only reach this case if the variable is bound outside the
          -- current closure.
          if isGhcVoid x then
            error "Free variables of type void not yet supported."
           else do
            r <- mbFreshLocal (repType (Ghc.varType x)) mr
            -- Do not force @i@ -- must remain a thunk
            let i = expectJust "transVar" (Ghc.lookupVarEnv fvi x)
            return (insLoadFV r i, r, in_whnf,
                    updateLoc locs0 x (InVar r))

      | otherwise -> do  -- global variable
          this_mdl <- getThisModule
          let x' = toplevelId this_mdl x
          r <- mbFreshLocal (repType (Ghc.varType x)) mr
          {- trace ("VARadd:" ++ ppVar x ++ " : " ++ pretty x') $ do -}
          return (insLoadGbl r x', r, isGhcConWorkId x,  -- TODO: only if CAF
                  updateLoc locs0 x (InVar r))
    r -> error $ "transVar: unhandled case: " ++ show r ++ " "
              ++ showPpr x
 where
   in_whnf = Ghc.isUnLiftedType (Ghc.idType x)

------------------------------------------------------------------------

loadDataCon :: DataCon -> LocalEnv -> FreeVarsIndex -> KnownLocs
            -> Maybe BcVar
            -> Trans (Bcis O, KnownLocs, BcVar)
loadDataCon dcon env fvi locs0 mr = do
  let x = dataConWorkId dcon
  case lookupItblLoc locs0 x of
    Just (InVar x') ->
      {- trace ("DCONinVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ -}
      return (mbMove mr x', locs0, fromMaybe x' mr)
    Just (InReg r ty) -> do
      -- trace ("DCONinREG (" ++ pretty r ++ ") <> " ++ ppVar  x) $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType ty)),
              updateItblLoc locs0 x (InVar x'), x')
    Nothing -> do
      this_mdl <- getThisModule
      let is_nullary = Ghc.isNullarySrcDataCon dcon
      let x' | is_nullary = toplevelId this_mdl x
             | otherwise  = dataConInfoTableId dcon
      let ty | is_nullary = ghcAnyType
             | otherwise  = Ghc.bcoPrimTy
      -- trace ("DCONadd: " ++ ppVar x ++ " : " ++ pretty x') $ do
      r <- mbFreshLocal ty mr
      return (insLoadGbl r x',
              updateItblLoc locs0 x (InVar r),
              r)-- TODO: only if CAF

------------------------------------------------------------------------

-- | Append a @Ret1@ instruction if needed and return.
maybeAddRet :: Context x -> Bcis O -> KnownLocs -> BcVar
            -> Trans (Bcis x, KnownLocs, Maybe BcVar)
maybeAddRet (BindC _) is locs r =
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
  Ghc.MachStr fs   -> CStr (unpackFS fs)
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
