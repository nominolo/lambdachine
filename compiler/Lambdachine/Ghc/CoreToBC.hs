{-# LANGUAGE ViewPatterns, GADTs, ScopedTypeVariables, CPP #-}
{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving, BangPatterns #-}
{-| Generate bytecode from GHC Core.

GHC Core has quite a few invariants and accommodating them all
can be quite difficult (and fragile).  Fortunately, GHC provides
a clean-up transformation called @CorePrep@ that essentially transforms
Core into A-normal form.  The grammar for the output is:

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

Boquist's GRIN used different translation schemes for generating code
in a strict context and in a lazy context.  The latter would just
build a thunk.  We don't need this here, because thunks are build
using @let@ expressions.

The translation scheme still passes around a 'Context' argument, but
that is used mainly to detect tail calls.

Our bytecode does not support @let@ statements.  Nested bindings are
translated into top-level bindings and matching allocation
instructions at the original binding site.

 -}
module Lambdachine.Ghc.CoreToBC where

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
import CoreSyn ( CoreBind, CoreBndr, CoreExpr, CoreArg, CoreAlt,
                 Bind(..), Expr(..),
                 AltCon(..),
                 collectBinders, flattenBinds, collectArgs )
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

----------------------------------------------------------------------
-- * Debug Utils:

unimplemented :: String -> a
unimplemented str = error $ "UNIMPLEMENTED: " ++ str

tracePpr :: Outputable a => a -> b -> b
tracePpr o exp = trace (">>> " ++ showPpr o) exp

showPpr1 :: Outputable a => a -> String
showPpr1 o = Out.showDocWith Out.OneLineMode $
  Out.withPprStyleDoc tracingDynFlags Out.defaultUserStyle (Out.ppr o)

-- -------------------------------------------------------------------
-- * Top-level Interface
type Bcis x = BcGraph O x

-- | Generate bytecode for the given bindings and type constructors.
--
-- This is the main interface to the bytecode compiler.
generateBytecode ::
     Supply Unique  -- ^ Used for generating identifiers.
  -> Ghc.ModuleName -- ^ The name of the module being compiled.
  -> [CoreBind]  -- ^ Bindings defined in the module
  -> [TyCon]     -- ^ Type constructors defined in this module.
  -> BCOs -- ^ The bytecode objects for this module.
generateBytecode us mdl bndrs0 data_tycons =
  runTrans mdl us $ do
    let dcon_bcos = M.unions $ map transTyCon data_tycons
    toplevel_bcos <- go bndrs0 mempty
    local_bcos <- getBCOs
    return (M.unions [dcon_bcos, toplevel_bcos, local_bcos])
 where
--   go _ _ | trace "genBC-go" False = undefined
   go [] acc = return acc
   go (NonRec f body : bndrs) acc = do
     bcos <- transTopLevelBind f body
     go bndrs (M.union bcos acc)
   go (Rec fs : bndrs) acc =
     let go' [] acc' = go bndrs acc'
         go' ((f, body):fs') acc' = do
           bcos <- transTopLevelBind f body
           go' fs' (M.union bcos acc')
     in go' fs acc

-- | Translate a single type constructor into 'BCOs'.
--
-- This creates one object for describing the type itself and
-- one object per data constructor of the type.
--
transTyCon :: TyCon -> BCOs
transTyCon tycon = do
  let bcos0 =
        M.singleton (tyConId (Ghc.tyConName tycon))
           BcTyConInfo{ bcoDataCons =
                          map dataConInfoTableId (Ghc.tyConDataCons tycon) }
  collect' bcos0 (Ghc.tyConDataCons tycon) $ \bcos dcon ->
    let dcon_id = dataConInfoTableId dcon
        ty = transType (Ghc.dataConRepType dcon)
        arg_tys | FunTy args _ <- ty = args
                | otherwise = []
        bco = BcConInfo { bcoConTag = Ghc.dataConTag dcon
                        , bcoConFields = Ghc.dataConRepArity dcon
                        , bcoConArgTypes = arg_tys }
    in M.insert dcon_id bco bcos

repType :: a -> a
repType = id

-- | Translate a top-level binding.
transTopLevelBind :: CoreBndr -> CoreExpr -> Trans BCOs
transTopLevelBind f (viewGhcLam -> (params, body)) = do
  this_mdl <- getThisModule
  let !f' = toplevelId this_mdl f
  let bco_type | (_:_) <- params =
                 BcoFun (length params)
                        (map (transType . repType . Ghc.varType) params)
               | looksLikeCon body = Con
               | isGhcConWorkId f = Con
               | otherwise = CAF
  case bco_type of
    Con -> buildCon f' body
    _ -> do
      let env0 = mkLocalEnv [(x, undefined) | x <- params]
          locs0 = mkLocs [ (b, InReg n t)
                         | (b, n) <- zip params [0..]
                         , let t = repType (Ghc.varType b) ]
          fvi0 = Ghc.emptyVarEnv
      (bcis, _, fvs, Nothing) <- withParentFun f $ transBody body env0 fvi0 locs0 RetC
      g <- finaliseBcGraph bcis
      let bco = BcObject { bcoType = bco_type
                         , bcoCode = g
                         , bcoGlobalRefs = toList (globalVars fvs)
                         , bcoConstants = []
                         , bcoFreeVars = M.empty
                         }
      return (M.singleton f' bco)

-- | 'True' iff the input expression looks like a type constructor
-- application with at least one argument.
--
-- Constructors with zero arguments are treated specially, so this
-- function will return 'False' for them.
looksLikeCon :: CoreExpr -> Bool
looksLikeCon (viewGhcApp -> Just (f, args)) = 
  not (null args) && isGhcConWorkId f 
looksLikeCon _ = False

-- | Build the 'BCO' for a statically allocated constructor (i.e.,
-- data).
buildCon :: Id -> CoreExpr -> Trans BCOs
buildCon f (viewGhcApp -> Just (dcon, args0)) = do
  this_mdl <- getThisModule
  let dcon' = dataConInfoTableId (ghcIdDataCon dcon)
      fields = transFields (toplevelId this_mdl) args0
  return (M.singleton f (BcoCon Con dcon' fields))

-- | A thing that can be stored on the heap
data ClosureInfo
  = ConObj Ghc.Id [CoreArg]
    -- ^ Arguments @Dcon@ and @a1 ... aN@ represents a closure
    -- allocated as:
    --
    -- > let x = DCon a1 .. aN in ...
    --
    -- The constructor is fully applied.
  | AppObj Ghc.Id [CoreArg]
    -- ^ Arguments @f@ and @a1 ... aN@ represents a closure allocated
    -- as:
    --
    -- > let x = f a1 .. aN in ...
    --
  | FunObj !Int Id [Ghc.Id] Ghc.Type
    -- ^ Argumenst @N@ (arity), @f@, and @y1 ... yM@.
    --
    -- > let f = \\x1 .. xN -> ... in ...
    --
    -- The function body has been translated and assigned name @f@.
    -- Arity is the number of arguments of the function.  The @y@
    -- arguments are the free variables which must be stored in the
    -- heap object.

-- | Translate a local binding.
--
-- For a simple non-recursive binding we distinguish three forms of
-- the RHS:
--
--   * @C x1 .. xN@: Constructor application.  This is
--     translated into a direct @ALLOC@ instruction.
--
--   * @f x1 .. xN@: Regular function application.  GHC creates custom
--     code for each application.  Since we're paying the bytecode
--     overhead already, however, we can just create a generic @AP@
--     node.  The JIT will automatically generate specialised code for
--     commonly encountered @AP@ nodes (or optimise away their
--     creation in the first place).
--
--   * For anything else we create a new top-level BCO.
--
-- In each case we keep track of the free variables referenced by the RHS
-- and allocate the proper object at the original binding site.
--
-- In the case of recursive bindings matters get slightly more
-- complicated.  Consider the following code:
--
-- > let x = Cons a y
-- >     y = Cons b x
-- > in ...
--
-- Note that the first reference to @y@ is actually a forward
-- reference, that is, the value of @y@ is not known at the point
-- where it is needed to initialise the object stored in @x@.
--
-- There are (at least) two options to translate this into bytecode:
--
-- Option 1: Write a dummy value into the @x@ object and then update
-- it later when the value of @y@ is known:
--
-- > LOADCON r1, Cons
-- > LOADCON r2, Blackhole
-- > ALLOC r3, r1, <a>, r2  -- allocate (Cons a *)
-- > ALLOC r4, r1, <b>, r3  -- allocate (Cons b x)
-- > STORE r3, 2, r4        -- fixup second field of x
--
-- Option 2: Separate allocation from initialisation.
--
-- > ALLOC r3, Cons
-- > ALLOC r4, Cons
-- > STORE r3, 1, <a>
-- > STORE r3, 2, r4
-- > STORE r4, 1, <b>
-- > STORE r4, 2, r3
--
-- It is not obvious which variant is better.  Option 2 requires that
-- the allocation and initialisation sequence is non-interruptible.
-- Otherwise, the fields freshly allocated objects must be initialised
-- to a dummy value in order to avoid confusing the garbage collector.
-- For that reason we currently use Option 1.
-- 
transBinds :: CoreBind -> LocalEnv -> FreeVarsIndex 
           -> KnownLocs
           -> Trans (Bcis O, KnownLocs, FreeVars, LocalEnv)
transBinds bind env fvi locs0 = do
  case bind of
    NonRec f body -> do
      ci <- transBind f body env
      build_bind_code Ghc.emptyVarEnv (extendLocalEnv env f undefined)
                      fvi [(f, ci)] locs0
    Rec bndrs -> do
      let xs = map fst bndrs
          env' = extendLocalEnvList env xs
      (cis, fw_env) <- trans_binds_rec (Ghc.mkVarSet xs) bndrs env' Ghc.emptyVarEnv []
      let locs1 = extendLocs locs0 [ (x, Fwd) | x <- xs ]
      build_bind_code fw_env env' fvi cis locs1
 where
   trans_binds_rec :: Ghc.VarSet -- variables that will be defined later
                   -> [(CoreBndr, CoreExpr)] -- the bindings
                   -> LocalEnv  -- the local env
                   -> Ghc.VarEnv [(Int, Ghc.Id)] -- fixup accumulator
                   -> [(Ghc.Id, ClosureInfo)] -- closure info accumulator
                   -> Trans ([(Ghc.Id, ClosureInfo)],
                             Ghc.VarEnv [(Int, Ghc.Id)])
   trans_binds_rec _fwds [] _env fix_acc ci_acc =
     return (reverse ci_acc, fix_acc)
   trans_binds_rec fwds ((f, body):binds) env fix_acc ci_acc = do
     closure_info <- transBind f body env
     let fwd_refs =
           case closure_info of
             ConObj dcon fields ->
               [ (x, offs) | (offs, Right x) <- zip [1..] (map viewGhcArg fields)
                           , x `Ghc.elemVarSet` fwds ]
             AppObj doc fields ->
               [ (x, offs) | (offs, Right x) <- zip [2..] (map viewGhcArg fields)
                           , x `Ghc.elemVarSet` fwds ]
             FunObj _ _ frees _ ->
               [ (x, offs) | (offs, x) <- zip [1..] frees
                           , x `Ghc.elemVarSet` fwds ]
     let fix_acc' = foldl' (\e (x, offs) ->
                              Ghc.extendVarEnv_C (++) e x [(offs, f)])
                      fix_acc fwd_refs
     let fwds' = Ghc.delVarSet fwds f
     trans_binds_rec fwds' binds env fix_acc'
                     ((f, closure_info) : ci_acc)

-- | Creates the actual code sequence (including fixup of forward references).
build_bind_code :: Ghc.VarEnv [(Int, Ghc.Id)]
                -> LocalEnv -> FreeVarsIndex
                -> [(Ghc.Id, ClosureInfo)] -> KnownLocs
                -> Trans (Bcis O, KnownLocs, FreeVars, LocalEnv)
build_bind_code fwd_env env fvi closures locs0 = do
  (bcis, locs, fvs) <- go emptyGraph locs0 mempty closures
  return (bcis, locs, fvs, env)
 where
   -- TODO: Do we need to accumulate the environment?
   go bcis locs fvs [] = return (bcis, locs, fvs)

   -- r = alloc dcon(f1,...,fN)
   go bcis locs0 fvs ((x, ConObj dcon fields) : objs) = do
     (bcis1, locs1, fvs1, Just r)
       <- transStore dcon fields env fvi locs0 (BindC Nothing)
     let locs2 = updateLoc locs1 x (InVar r)
     go (bcis <*> bcis1 <*> add_fw_refs x r locs2) locs2 (fvs `mappend` fvs1) objs

   -- r = r2
   go bcis locs0 fvs ((x, AppObj f []) : objs) = do
     (bcis1, locs1, fvs1, [fvar]) <- transArgs [Ghc.Var f] env fvi locs0
     xvar <- mbFreshLocal (repType (Ghc.varType x)) Nothing
     let bcis2 = bcis <*> bcis1 <*> insMove xvar fvar
         locs2 = updateLoc locs1 x (InVar xvar)
     go bcis2 locs2 (fvs `mappend` fvs1) objs
 
   -- r = allocap f(a1,...,aN)
   go bcis locs0 fvs ((x, AppObj f args) : objs) = do
     (bcis1, locs1, fvs1, (freg:regs))
       <- transArgs (Ghc.Var f : args) env fvi locs0
     let Just (arg_tys, rslt_ty) =
           --trace ("splitfun" ++
           --       ghcPretty (length args, f, repType (Ghc.varType f),
           --              Ghc.splitFunTys (repType (Ghc.varType f)))) $
            splitFunTysN (length args) $
               repType (Ghc.varType f)
     -- trace ("AppObj:" ++ show (length args) ++ ":" ++
     --        Ghc.showSDoc (Ghc.ppr (repType (Ghc.varType f))) ++
     --        " => " ++ Ghc.showSDoc (Ghc.ppr rslt_ty)) $ do
     rslt <- mbFreshLocal rslt_ty Nothing
     let bcis2 = (bcis <*> bcis1) <*> insMkAp rslt (freg:regs)
         locs2 = updateLoc locs1 x (InVar rslt)
         bcis3 = bcis2 <*> add_fw_refs x rslt locs2
     go bcis3 locs2 (fvs `mappend` fvs1) objs

   -- allocate a thunk
   go bcis locs0 fvs ((x, FunObj _arity info_tbl0 args ty) : objs)
    | length args > 0 = do
     let info_tbl = mkInfoTableId (idName info_tbl0)
     (bcis1, locs1, fvs1, regs)
       <- transArgs (map Ghc.Var args) env fvi locs0
--      trace ("FunTy:" ++ ghcPretty x ++ ": " ++ ghcPretty args ++ " % " ++
--             ghcPretty ty) $ do
     -- We kind of abuse GHC's BCO type here, because we don't need
     -- anything equivalent to it
     let tag_type = Ghc.bcoPrimTy
     tag_reg <- mbFreshLocal tag_type Nothing
     rslt <- mbFreshLocal ty Nothing
     let bcis2 = bcis <*> bcis1 <*> insLoadGbl tag_reg info_tbl <*>
                 insAlloc rslt tag_reg regs
         locs2 = updateLoc locs1 x (InVar rslt)
         bcis3 = bcis2 <*> add_fw_refs x rslt locs2
     go bcis3 locs2 (fvs `mappend` fvs1) objs

   -- If args is [], then we are loading a reference to a CAF, no
   -- allocation necessary.
   go bcis0 locs0 fvs (obj@(x, FunObj _arity info_tbl0 args ty) : objs)
    | otherwise = do
     reg <- mbFreshLocal ty Nothing
     let bcis2 = bcis0 <*> insLoadGbl reg (mkTopLevelId (idName info_tbl0))
         locs2 = updateLoc locs0 x (InVar reg)
     go bcis2 locs2 fvs objs

   add_fw_refs x r locs =
     case Ghc.lookupVarEnv fwd_env x of
       Nothing -> emptyGraph --emptyBag
       Just fixups ->
         catGraphs $   -- listToBag $
           [ insStore ry n r | (n, y) <- fixups,
                               let Just (InVar ry) = lookupLoc locs y ]

-- | Translate a (non-toplevel) binding.
transBind :: CoreBndr -- ^ The binder name ...
          -> CoreExpr -- ^ ... and its body.
          -> LocalEnv -- ^ The 'LocalEnv' at the binding site.
          -> Trans ClosureInfo
transBind x (viewGhcApp -> Just (f, args)) _env0
 | isGhcConWorkId f
 = return (ConObj f args)
 | Just _ <- isGhcPrimOpId f
 = error $ "Primop in let: " ++ showPpr x
 | otherwise
 = return (AppObj f args)
transBind x (viewGhcLam -> (bndrs, body)) env0 = do
  let locs0 = mkLocs $ (x, Self) : [ (b, InReg n t) |
                                     (b, n) <- zip bndrs [0..],
                                     let t = repType (Ghc.varType b) ]
      -- The new local environment does *not* include env0, because
      -- elements of env0 are no longer local in the body of the
      -- lambda.  They must be accessed explicitly via the free
      -- variable environment:
      env = fold2l' extendLocalEnv env0 bndrs (repeat undefined)

  -- Here comes the magic:
  (bcis, vars, gbls, _) <- mfix $ \ ~(_bcis, _vars, _gbls, fvis) -> do
    (bcis, locs', fvs, Nothing) <- withParentFun x $ transBody body env fvis locs0 RetC
    let closure_vars = Ghc.varSetElems (closureVars fvs)
        -- maps from closure variable to its index
        cv_indices = Ghc.mkVarEnv (zip closure_vars [(1::Int)..])
    return (bcis, closure_vars, globalVars fvs, cv_indices)

  this_mdl <- getThisModule
  parent <- getParentFun
  let cl_prefix | Nothing <- parent = ".cl_"
                | Just s  <- parent = ".cl_" ++ s ++ "_"
  x' <- freshVar (showPpr this_mdl ++ cl_prefix ++ Ghc.getOccString x) mkTopLevelId
  --trace ("DBG: " ++ show parent ++ "=>" ++ show x') $ do
  g <- finaliseBcGraph bcis
  let arity = length bndrs
      arg_types = map (transType . repType . Ghc.varType) bndrs
      free_vars = M.fromList [ (n, transType (Ghc.varType v))
                              | (n, v) <- zip [1..] vars ]
  let bco = BcObject { bcoType = if arity > 0 then
                                   BcoFun arity arg_types
                                  else if length vars > 0
                                        then Thunk else CAF
                     , bcoCode = g
                     , bcoConstants = []
                     , bcoGlobalRefs = toList gbls
                     , bcoFreeVars = free_vars }
  addBCO x' bco
  return (FunObj arity x' vars (repType (Ghc.varType x)))

-- | Split a list of variables into pointers and non-pointers.
splitPtrNonPtr :: [Ghc.Id] -> ([Ghc.Id], [Ghc.Id])
splitPtrNonPtr =
  -- assumes that 'partition' preserves the input ordering
  partition (isGCPointer . transType . Ghc.varType)

transFields :: (Ghc.Id -> a) -> [CoreArg] -> [Either BcConst a]
transFields f args = map to_field args
 where
   to_field (Ghc.Lit (Ghc.MachInt n)) = Left (CInt n)
   to_field (Ghc.Lit (Ghc.MachChar c)) = Left (CChar c)
   to_field (Ghc.Lit (Ghc.MachWord n)) = Left (CWord n)
   to_field (Ghc.Var x)               = Right (f x)
   to_field (Ghc.App x (Ghc.Type _))  = to_field x
   to_field (Lam a x) | isTyVar a     = to_field x
   to_field (Cast x _)                = to_field x
--   to_field (Note _ x)                = to_field x
   to_field arg = 
     error $ "transFields: Ill-formed argument: " ++ showPpr arg

-- -------------------------------------------------------------------

newtype Trans a = Trans { unTrans :: State TransState a }
  deriving (Functor, Applicative, Monad, MonadFix)

-- transFix :: (a -> Trans a) -> Trans a
-- transFix f = let Trans s = 

data TransState = TransState
  { tsUniques :: Supply Unique
  , tsLocalBCOs :: BCOs
  , tsModuleName :: Ghc.ModuleName
  , tsParentFun :: Maybe String
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

getThisModule :: Trans Ghc.ModuleName
getThisModule = Trans $ gets tsModuleName

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

getParentFun :: Trans (Maybe String)
getParentFun = Trans (gets tsParentFun)

instance UniqueMonad Trans where
  freshUnique = hooplUniqueFromUniqueSupply `fmap` genUnique

addBCO :: Id -> BytecodeObject -> Trans ()
addBCO f bco = Trans $
  modify' $ \s ->
    let !bcos' = M.insert f bco (tsLocalBCOs s) in
    s{ tsLocalBCOs = bcos' }

getBCOs :: Trans BCOs
getBCOs = Trans $ gets tsLocalBCOs

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


type FreeVarsIndex = Ghc.IdEnv Int

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


data FreeVars = FreeVars 
  { closureVars :: Ghc.VarSet  -- ^ Closure variables for this BCO
  , globalVars  :: S.Set Id  -- ^ References to global vars from this BCO.
  }

instance Monoid FreeVars where
  mempty = FreeVars Ghc.emptyVarSet S.empty
  (FreeVars cvs1 gvs1) `mappend` (FreeVars cvs2 gvs2) =
    FreeVars (cvs1 `Ghc.unionVarSet` cvs2) (gvs1 `S.union` gvs2)

closureVar :: Ghc.Id -> FreeVars
closureVar x = FreeVars (Ghc.unitVarSet x) S.empty

globalVar :: Id -> FreeVars
globalVar x = FreeVars Ghc.emptyVarSet (S.singleton x)

freshVar :: String -> (Name -> a) -> Trans a
freshVar nm f = do
  us <- genUnique
  return (f (freshName us (nm ++ tail (show (supplyValue us)))))

mbFreshLocal :: Ghc.Type -> Maybe BcVar -> Trans BcVar
mbFreshLocal _ (Just v) = return v
mbFreshLocal t Nothing = freshVar "%" (\n -> BcVar (mkLocalId n) t) 

-- | Create a new local 'Id' from a 'Ghc.Id'.
internCoreBndr :: CoreBndr -> Trans Id
internCoreBndr x = freshVar (Ghc.getOccString x) mkLocalId

-- | Translate a @body@ (using @CorePrep@ terminology) into bytecode.
--
-- If the context is 'RetC' will append a return statement or tail
-- call at the end.  Otherwise, the result is written into a register.
transBody ::
     CoreExpr  -- ^ The expression we're working on.
  -> LocalEnv  -- ^ The locally bound variables.  See 'LocalEnv'.
  -> FreeVarsIndex
  -> KnownLocs -- ^ Known register locations for some vars.
  -> Context x  -- ^ The code generation context.
  -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
     -- ^ Returns:
     --
     --  * The instructions corresponding to the expression
     --  * Modified register locations
     --  * Free variables (not top-level) of the expression.
     --  * The variable that the result is bound to (if needed).
     --

--transBody e _ _ _ _ | tracePpr e False = undefined
transBody (Ghc.Lit l) env _fvi locs ctxt = do
  (is, r) <- transLiteral l (contextVar ctxt)
  case ctxt of
    RetC -> return (is <*> insRet1 r,
                   locs, mempty, Nothing)
    BindC _ -> return (is, locs, mempty, Just r)

transBody (Ghc.Var x) env fvi locs0 ctxt = do
  (is0, r, eval'd, locs1, fvs) <- transVar x env fvi locs0 (contextVar ctxt)
  let is | eval'd = is0
         | otherwise = withFresh $ \l ->
                         is0 <*> insEval l r |*><*| mkLabel l
  case ctxt of
    RetC -> return (is <*> insRet1 r,
                   locs1, fvs, Nothing)
    BindC _ -> return (is, locs1, fvs, Just r)

transBody expr@(Ghc.App _ _) env fvi locs0 ctxt
 | Just (f, args) <- viewGhcApp expr
 = transApp f args env fvi locs0 ctxt

-- Special case for primitive conditions, i.e., (case x #< y of ...)
-- and such.
transBody (Ghc.Case scrut@(Ghc.App _ _) bndr alt_ty alts) env0 fvi locs0 ctxt
  | Just (f, args) <- viewGhcApp scrut,
    Just (cond, ty) <- isCondPrimOp =<< isGhcPrimOpId f,
    isLength 2 args
  = case alts of
      [_,_] -> transBinaryCase cond ty args bndr alt_ty alts
                               env0 fvi locs0 ctxt
      [_] ->
        transBody build_bool_expr env0 fvi locs0 ctxt
 where
   build_bool_expr =
     Ghc.Case
       (Ghc.Case scrut bndr Ghc.boolTy
          [(DataAlt Ghc.trueDataCon,  [], Ghc.mkConApp Ghc.trueDataCon [])
          ,(DataAlt Ghc.falseDataCon, [], Ghc.mkConApp Ghc.falseDataCon [])])
       bndr
       alt_ty
       alts
 
transBody (Ghc.Case scrut bndr ty alts) env0 fvi locs0 ctxt =
  transCase scrut bndr ty alts env0 fvi locs0 ctxt

transBody (Ghc.Let (NonRec x (viewGhcApp -> Just (f, args@(_:_)))) body)
          env fvi locs0 ctxt
 | isGhcConWorkId f
  = do (bcis0, locs1, fvs0, Just r)
         <- transStore f args env fvi locs0 (BindC Nothing)
       let locs2 = updateLoc locs1 x (InVar r)
           env' = extendLocalEnv env x undefined
       (bcis1, locs3, fvs1, mb_r) <- withParentFun x $ transBody body env' fvi locs2 ctxt
       return (bcis0 <*> bcis1, locs3, fvs0 `mappend` fvs1, mb_r)

transBody (Ghc.Let bind body) env fvi locs0 ctxt = do
  (bcis, locs1, fvs, env') <- transBinds bind env fvi locs0
  (bcis', locs2, fvs', mb_r) <- transBody body env' fvi locs1 ctxt
  return (bcis <*> bcis', locs2, fvs `mappend` fvs', mb_r)

--transBody (Ghc.Note _ e) env fvi locs ctxt = transBody e env fvi locs ctxt
transBody (Ghc.Cast e _) env fvi locs ctxt = transBody e env fvi locs ctxt
transBody (Ghc.Lam a e) env fvi locs ctxt
  | isTyVar a = transBody e env fvi locs ctxt
transBody e _ _ _ _ = error $ "transBody: " ++ showPpr e

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
  -> Trans (Bcis O, BcVar, Bool, KnownLocs, FreeVars)
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
     -- * TODO

-- transVar x _ _ _ _ | trace ("transVar: " ++ showPpr x ++ " : "
--                            ++ showPpr (Ghc.idType x) ++ " / "
--                            ++ show (not (Ghc.isUnLiftedType (Ghc.idType x))))
--                     False = undefined
transVar x env fvi locs0 mr =
  case lookupLoc locs0 x of
    Just (InVar x') -> {- trace ("inVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ do -}
      return (mbMove mr x', fromMaybe x' mr, in_whnf, locs0, mempty)
    Just (InReg r ty) -> do -- trace "inREG" $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType ty)), x', in_whnf,
              updateLoc locs0 x (InVar x'), mempty)
    Just (Field p n ty) -> do -- trace "inFLD" $ do
      --trace ("Field:" ++ show (p,bcVarType p,n) ++ ":" ++ ghcPretty ty) $ do
      r <- mbFreshLocal ty mr
      return (insFetch r p n,
              r, in_whnf, updateLoc locs0 x (InVar r), mempty)
    Just Fwd -> do
      -- A forward reference is replaced by a black hole and which
      -- does not need to be followed by anything.  So we give it a
      -- silly type.
      r <- mbFreshLocal Ghc.unitTy mr
      return (insLoadBlackhole r, r, True, locs0, mempty)
    Just Self -> do
      -- TODO: Find out the real type of Self closure?  It's always a
      -- pointer so (Any :: *) should be fine for now.
      r <- mbFreshLocal ghcAnyType mr
      return (insLoadSelf r, r, True, locs0, mempty)
    Just Void -> do
      r <- mbFreshLocal Ghc.realWorldStatePrimTy mr
      return (emptyGraph, r, True, locs0, mempty)
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
                    updateLoc locs0 x (InVar r), closureVar x)

      | otherwise -> do  -- global variable
          this_mdl <- getThisModule
          let x' = toplevelId this_mdl x
          r <- mbFreshLocal (repType (Ghc.varType x)) mr
          {- trace ("VARadd:" ++ ppVar x ++ " : " ++ pretty x') $ do -}
          return (insLoadGbl r x', r, isGhcConWorkId x,  -- TODO: only if CAF
                  updateLoc locs0 x (InVar r), globalVar x')
    r -> error $ "transVar: unhandled case: " ++ show r ++ " "
              ++ showPpr x
 where
   in_whnf = Ghc.isUnLiftedType (Ghc.idType x)

-- | Return the (GHC) type of the given variable which must not be a
-- register.
bcVarType :: BcVar -> Ghc.Type
bcVarType (BcVar _ t) = t
bcVarType (BcReg _ _) = error "bcVarType: Not a variable but a register"

transApp :: CoreBndr -> [CoreArg] -> LocalEnv -> FreeVarsIndex
         -> KnownLocs -> Context x
         -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
transApp f [] env fvi locs ctxt = transBody (Ghc.Var f) env fvi locs ctxt
transApp f args env fvi locs0 ctxt
  | length args > cMAX_CALL_ARGS
  = error $ "Call with too many args: " ++ showPpr f ++ " (" ++
            show (length args) ++ ")"
  | Just p <- isGhcPrimOpId f
  = do (is0, locs1, fvs, regs) <- transArgs args env fvi locs0
       case () of
         _ | Just (op, ty) <- primOpToBinOp p, [r1, r2] <- regs
           -> do
             let Just (_arg_tys, rslt_ty) =
                   splitFunTysN 2 $ repType (Ghc.varType f)
             rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
             maybeAddRet ctxt (is0 <*> insBinOp op ty rslt r1 r2)
                         locs1 fvs rslt
         _ | Just (cond, ty) <- isCondPrimOp p, [r1, r2] <- regs
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
             rslt <- mbFreshLocal Ghc.boolTy (contextVar ctxt)
             l1 <- freshLabel;  l2 <- freshLabel;  l3 <- freshLabel
             let is1 =  -- shape: O/O
                   catGraphsC (is0 <*> insBranch cond ty r1 r2 l1 l2)
                     [ mkLabel l1 <*> insLoadGbl rslt trueDataConId
                                  <*> insGoto l3,
                       mkLabel l2 <*> insLoadGbl rslt falseDataConId
                                  <*> insGoto l3]
                   |*><*| mkLabel l3
             maybeAddRet ctxt is1 locs1 fvs rslt

         _ | Just (OpNop, [arg_ty], res_ty) <- primOpOther p
           -> do
             -- Nop-like-primitives translate into a Move which gets
             -- optimised away by the register allocator (most
             -- likely).
             let [reg] = regs
             let Just(_argument_types, result_type) =
                   splitFunTysN 1 (repType (Ghc.varType f))
             result <- mbFreshLocal result_type (contextVar ctxt)
             maybeAddRet ctxt (is0 <*> insMove result reg) locs1 fvs result
             
         _ | Just (op, arg_tys, res_ty) <- primOpOther p
           -> do
             let arity = length arg_tys
             when (arity /= length regs) $
               error $ "Wrong number of primitive args.  Got = "
                     ++ show (length regs) ++ " expected = " ++ show arity
             -- TODO: We could type check the arguments as an extra assertion.
             -- That's a bit tricky given the current setup, though.
             let Just(_argument_types, result_type) =
                   splitFunTysN arity (repType (Ghc.varType f))
             result <- mbFreshLocal result_type (contextVar ctxt)
             maybeAddRet ctxt (is0 <*> insPrimOp op res_ty result regs)
                         locs1 fvs result

         _ | otherwise ->
             error $ "Unknown primop: " ++ showPpr p

  | isGhcConWorkId f  -- allocation
  = transStore f args env fvi locs0 ctxt

  | otherwise
  = do (is0, locs1, fvs0, regs) <- transArgs args env fvi locs0
       (is1, fr, _, locs2, fvs1)
          <- transVar f env fvi locs1 Nothing
       let is2 = is0 <*> is1
           fvs = fvs0 `mappend` fvs1
       case ctxt of
         RetC -> -- tailcall
           -- Ensure that tailcalls always use registers r0..r(N-1)
           -- for arguments.  This allows zero-copy function call.
           let typed_regs = [ BcReg n (transType (bcVarType r))
                            | (n,r) <- zip [0..] regs ]
               is = is2 <*>
                      catGraphs [ insMove tr r 
                                | (tr,r) <- zip typed_regs regs ]
               ins = is <*> insCall Nothing fr typed_regs
           in
           return (ins, locs2, fvs, Nothing)
         BindC mr ->  do
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
           r <- mbFreshLocal rslt_ty mr
           let ins = withFresh $ \l ->
                       is2 <*> insCall (Just (r, l)) fr regs |*><*| mkLabel l
           return (ins, locs2, fvs, Just r)
--error $ "transApp: " ++ showPpr f ++ " " ++ showPpr (Ghc.idDetails f)

-- | Generate code for loading the given function arguments into
-- registers.
transArgs :: [CoreArg] -> LocalEnv -> FreeVarsIndex -> KnownLocs
          -> Trans (Bcis O, KnownLocs, FreeVars, [BcVar])
transArgs args0 env fvi locs0 = go args0 emptyGraph locs0 mempty []
 where
   go [] bcis locs fvs regs = return (bcis, locs, fvs, reverse regs)
   go (arg:args) bcis locs fvs regs = do
     (bcis', locs', fvs', r) <- trans_arg arg locs
     go args (bcis <*> bcis') locs' (fvs `mappend` fvs') (r:regs)

   trans_arg (Ghc.Lit l) locs = do
     (bcis, r) <- transLiteral l Nothing
     return (bcis, locs, mempty, r)
   trans_arg (Ghc.Var x) locs = do
     (bcis, r, _, locs', fvs) <- transVar x env fvi locs Nothing
     return (bcis, locs', fvs, r)
   -- The boring cases
   trans_arg (Ghc.App x (Ghc.Type _)) locs = trans_arg x locs
   trans_arg (Lam a x) locs | isTyVar a    = trans_arg x locs
   trans_arg (Cast x _) locs               = trans_arg x locs
--   trans_arg (Note _ x) locs               = trans_arg x locs

transStore :: CoreBndr -> [CoreArg] -> LocalEnv -> FreeVarsIndex
           -> KnownLocs -> Context x
           -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
transStore dcon [] env fvi locs0 ctxt = -- bloody hack Since we only
  -- have saturated DataCon applications this must be a zero-arity
  -- constructor (e.g., Nil).  These don't require any allocation,
  -- they're basically distinguished pointers.
  transBody (Ghc.Var dcon) env fvi locs0 ctxt

transStore dcon0 args env fvi locs0 ctxt
 | dcon <- ghcIdDataCon dcon0, Ghc.isUnboxedTupleCon dcon
 = do
    case ctxt of
      BindC _ -> error "Trying to bind an unboxed tuple to a variable"
      RetC -> do
        (bcis, locs, fvs, vars0) <- transArgs args env fvi locs0
        let vars = removeIf isVoid vars0
        case vars of
          [res] -> 
            return (bcis <*> insRet1 res, locs, fvs, Nothing)
          (_:_:_) -> do
            let
              resultRegs =
                [ BcReg n (transType (bcVarType var))
                | (n, var) <- zip [0..] vars ]
              bcis' =
                bcis <*> catGraphs [ insMove reg var
                                   | (reg, var) <- zip resultRegs vars ]
            return (bcis' <*> insRetN resultRegs, locs, fvs, Nothing)

transStore dcon args env fvi locs0 ctxt = do
  (bcis0, locs1, fvs, regs) <- transArgs args env fvi locs0
  
  (bcis1, con_reg, locs2, fvs')
    <- loadDataCon dcon env fvi locs1 (contextVar ctxt)
  let Just (arg_tys, rslt_ty) =
        splitFunTysN (length args) $ repType (Ghc.varType dcon)
  rslt <- mbFreshLocal rslt_ty (contextVar ctxt)
  let bcis = (bcis0 <*> bcis1) <*> insAlloc rslt con_reg regs
  maybeAddRet ctxt bcis locs2 (fvs `mappend` fvs') rslt

ppVar :: CoreBndr -> String
ppVar x = Ghc.showSDocDebug tracingDynFlags (Ghc.ppr x) ++ "{" ++ show (getUnique x) ++ "}"

loadDataCon :: CoreBndr -> LocalEnv -> FreeVarsIndex -> KnownLocs -> Maybe BcVar
            -> Trans (Bcis O, BcVar, KnownLocs, FreeVars)
loadDataCon x env fvi locs0 mr = do
  case lookupItblLoc locs0 x of
    Just (InVar x') -> {- trace ("DCONinVAR: (" ++ pretty x' ++ ") <> " ++ ppVar x) $ -}
      return (mbMove mr x', fromMaybe x' mr, locs0, mempty)
    Just (InReg r ty) -> do -- trace ("DCONinREG (" ++ pretty r ++ ") <> " ++ ppVar  x) $
      x' <- mbFreshLocal ty mr
      return (insMove x' (BcReg r (transType ty)), x',
              updateItblLoc locs0 x (InVar x'), mempty)
    Nothing -> do
      this_mdl <- getThisModule
      let is_nullary = Ghc.isNullarySrcDataCon (ghcIdDataCon x)
      let x' | is_nullary = toplevelId this_mdl x
             | otherwise  = dataConInfoTableId (ghcIdDataCon x)
      let ty | is_nullary = ghcAnyType
             | otherwise  = Ghc.bcoPrimTy
      -- trace ("DCONadd: " ++ ppVar x ++ " : " ++ pretty x') $ do
      r <- mbFreshLocal ty mr
      return (insLoadGbl r x', r, -- TODO: only if CAF
                  updateItblLoc locs0 x (InVar r), globalVar x')

-- | Translate a case expression.
--
-- The scrutinee must not be of the shape @x <# y@ or any other
-- primitive binop.  These are handled by 'transBody'.
--
-- The translation is different depending on the form of the
-- case branches.
--
-- /Single-constructor/.  If there is only one constructor, the case
-- amounts to just loading one or more fields of the scrutinee.
--
-- /Literal patterns/.  A case matching on a number of literals is
-- translated into a binary decision tree.  See 'buildCaseTree'.
--
-- Any other case expression is translated into the appropriate
-- @CASE@ instruction.
transCase :: forall x.
             CoreExpr -> CoreBndr -> Ghc.Type -> [CoreAlt]
          -> LocalEnv -> FreeVarsIndex -> KnownLocs
          -> Context x
          -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)

-- Only a single case alternative.  This is just EVAL(bndr) and
-- possibly matching on the result.
transCase scrut bndr alt_ty [(altcon, vars, body)] env0 fvi locs0 ctxt 
 | DataAlt con <- altcon, Ghc.isUnboxedTupleCon con
 = do
  let nonVoidVars = removeIf isGhcVoid vars
  (bcis, locs1, fvs0, Just result0) <- transBody scrut env0 fvi locs0 (BindC Nothing)

  if (bndr `isFreeExprVarIn` body) then
    error $ "transCase: Binder in unboxed tuple is not a wildcard: " ++ showPpr (bndr, scrut, altcon, vars, body)
   else case nonVoidVars of
    [var] -> do  -- effectively only one value is actually returned
      let locs2 = updateLoc locs1 var (InVar result0)
          env' = extendLocalEnv env0 var undefined
      (bcis', locs3, fvs1, mb_r) <- transBody body env' fvi locs2 ctxt
      return (bcis <*> bcis', locs3, fvs0 `mappend` fvs1, mb_r)

    resultVar0:(otherResultVars@(_:_)) -> do
      -- Leave `bndr` undefined.  It should always be a wildcard.
      let env' = extendLocalEnvList env0 nonVoidVars

      -- Result variables don't survive across multiple CALL instructions
      -- so we load them all into fresh variables.
      regs <- mapM (\x -> mbFreshLocal (repType (Ghc.varType x)) Nothing)
                   otherResultVars
      let bcis1 = [ insLoadExtraResult r n | (r, n) <- zip regs [1..] ]
      let locs2 = extendLocs locs1 [(resultVar0, InVar result0)]
      let locs3 = extendLocs locs2
                    [ (x, InVar r) | (x, r) <- zip otherResultVars regs ]

      (bcis', locs4, fvs1, mb_r) <- transBody body env' fvi locs3 ctxt
      return (bcis <*> catGraphs bcis1 <*> bcis', locs4,
              fvs0 `mappend` fvs1, mb_r)
      
 | otherwise
 = do
  (bcis, locs1, fvs0, Just r) <- transBody scrut env0 fvi locs0 (BindC Nothing)
  let locs2 = updateLoc locs1 bndr (InVar r)
      env = extendLocalEnv env0 bndr undefined
  let locs3 = addMatchLocs locs2 r altcon vars
      env' = extendLocalEnvList env vars
  (bcis', locs4, fvs1, mb_r) <- transBody body env' fvi locs3 ctxt
  return (bcis <*> bcis', locs4, fvs0 `mappend` fvs1, mb_r)

-- Literal cases are handled specially by translating them into a
-- decision tree.  There's still room for improvement, though.  See
-- 'buildCaseTree' below.
transCase scrut bndr alt_ty alts env0 fvi locs0 ctxt
 | isLitCase alts
 = do
  (bcis0, locs1, fvs0, Just reg) <- transBody scrut env0 fvi locs0 (BindC Nothing)
  -- bndr gets bound to the literal
  let locs2 = updateLoc locs1 bndr (InVar reg)
      env = extendLocalEnv env0 bndr undefined
  let (dflt, ty, tree) = buildCaseTree alts

  -- If the context requires binding to a variable, then we have to
  -- make sure all branches write their result into the same
  -- variable.
  ctxt' <- (case ctxt of
             RetC -> return RetC
             BindC mr -> BindC . Just <$> mbFreshLocal alt_ty mr)
            :: Trans (Context x)

  end_label <- freshLabel

  let
    transArm :: CoreExpr -> Trans (Label, BcGraph C C, FreeVars)
    transArm bdy = do
      l <- freshLabel
      (bcis, _locs', fvs, _mb_var)
        <- transBody bdy env fvi locs2 ctxt'
      case ctxt' of
        RetC ->
          return (l, mkLabel l <*> bcis, fvs)
        BindC _ ->
          return (l, mkLabel l <*> bcis <*> insGoto end_label, fvs)

  (dflt_label, dflt_bcis, dflt_fvs) <- transArm dflt

  let
    build_branches :: CaseTree
                   -> Trans (Label, [BcGraph C C], FreeVars)

    build_branches (Leaf Nothing) = do
      return (dflt_label, [], mempty)
    build_branches (Leaf (Just expr)) = do
      (lbl, bci, fvs) <- transArm expr
      return (lbl, [bci], fvs)

    build_branches (Branch cmp lit true false) = do
      (true_lbl, true_bcis, true_fvs) <- build_branches true
      (false_lbl, false_bcis, false_fvs) <- build_branches false
      -- Ensure the code blocks are closed at the end
      (lit_bcis, lit_reg) <- transLiteral lit Nothing
      l <- freshLabel
      return (l, [mkLabel l <*> lit_bcis
                  <*> insBranch cmp ty reg lit_reg true_lbl false_lbl]
                 ++ true_bcis ++ false_bcis,
              true_fvs `mappend` false_fvs)

  case ctxt' of
    RetC -> do
      (l_root, bcis, fvs1) <- build_branches tree
      return ((bcis0 <*> insGoto l_root) `catGraphsC` bcis
               |*><*| dflt_bcis,
              locs1, mconcat [fvs0, fvs1, dflt_fvs], Nothing)
    BindC (Just r) -> do
      (l_root, bcis, fvs1) <- build_branches tree
      return ((bcis0 <*> insGoto l_root) `catGraphsC` bcis
                |*><*| dflt_bcis |*><*| mkLabel end_label,
              locs1, mconcat [fvs0, fvs1, dflt_fvs], Just r)

-- The general case
transCase scrut bndr alt_ty alts env0 fvi locs0 ctxt = do
  (bcis, locs1, fvs0, Just r) <- transBody scrut env0 fvi locs0 (BindC Nothing)
  let locs2 = updateLoc locs1 bndr (InVar r)
  let env = extendLocalEnv env0 bndr undefined
  let (tycon, _) = Ghc.splitTyConApp (Ghc.idType bndr)
  let tags = length (Ghc.tyConDataCons tycon)
  case ctxt of
    RetC -> do -- inss are closed at exit
      (alts, inss, fvs1) <- transCaseAlts alts r env fvi locs2 RetC
      return ((bcis <*> insCase (CaseOnTag tags) {- XXX: wrong -} r alts)
              `catGraphsC` inss,
              locs1, fvs0 `mappend` fvs1, Nothing)
    BindC mr -> do -- close inss' first
      -- error "UNTESTED"
      r1 <- mbFreshLocal alt_ty mr
      (alts, inss, fvs1) <- transCaseAlts alts r env fvi locs2 (BindC (Just r1))
      let bcis' =
            withFresh $ \l ->
              let inss' = [ ins <*> insGoto l | ins <- inss ] in
              ((bcis <*> insCase (CaseOnTag tags) r alts) `catGraphsC` inss')
               |*><*| mkLabel l  -- make sure we're open at the end
      return (bcis', locs1, fvs0 `mappend` fvs1, Just r1)


data CaseTree
  = Leaf (Maybe CoreExpr)  -- execute this code (or default)
  | Branch CmpOp Ghc.Literal CaseTree CaseTree
    -- cmp + ty,  true_case, false_case

-- | Given a list of literal pattern matches, builds a balanced tree.
--
-- The goal is for this tree to select among the @N@ alternatives in
-- @log2(N)@ time.
--
-- TODO: Detect and take advantage of ranges.
buildCaseTree :: [CoreAlt]
              -> (CoreExpr, OpTy, CaseTree)
                 -- ^ Default code, comparison type, and other cases
buildCaseTree ((DEFAULT, [], dflt_expr):alts0) =
  assert alts_is_sorted $ (dflt_expr, ty, buildTree alts)
 where
   alts = map simpl_lit alts0

   alts_is_sorted =
     map fst (sortBy (comparing fst) alts) == map fst alts

   dflt = Leaf Nothing
   leaf x = Leaf (Just x)

   simpl_lit (LitAlt lit, [], expr) =
     assert (ghcLiteralType lit == ty) $ (lit, expr)

   ty = case alts0 of ((LitAlt l, _, _):_) -> ghcLiteralType l

   buildTree [(l, body)] =
     Branch CmpEq l (leaf body) dflt
   buildTree [(l1, body1), (l2,body2)] =
     Branch CmpEq l1 (leaf body1) (Branch CmpEq l2 (leaf body2) dflt)
   buildTree alts1 =
     let l = length alts1 in
     case splitAt (l `div` 2) alts1 of
       (lows, highs@((l, _):_)) ->
         Branch CmpGe l (buildTree highs) (buildTree lows)

isLitCase :: [CoreAlt] -> Bool
isLitCase ((DEFAULT, _, _):alts) = isLitCase alts
isLitCase ((LitAlt _, _, _):_) = True
isLitCase ((DataAlt _, _, _):_) = False
isLitCase [] = False

transCaseAlts ::
     [CoreAlt] -- ^ The case alternatives
  -> BcVar     -- ^ The variable we're matching on.
  -> LocalEnv -> FreeVarsIndex -> KnownLocs -> Context x
  -> Trans ([(BcTag, BlockId)], [BcGraph C x], FreeVars)
transCaseAlts alts match_var env fvi locs0 ctxt = do
  (targets, bcis, fvss) <- unzip3 <$>
    (forM alts $ \(altcon, vars, body) -> do
      let locs1 = addMatchLocs locs0 match_var altcon vars
          env' = extendLocalEnvList env vars
      (bcis, _locs2, fvs, _mb_var) <- transBody body env' fvi locs1 ctxt
      l <- freshLabel
      return ((dataConTag altcon, l), mkLabel l <*> bcis, fvs))
  return (targets, bcis, mconcat fvss)

-- | Translate a binary case on a primop, i.e., a two-arm branch
-- with alternatives @True@ or @False@.
transBinaryCase :: forall x.
                   BinOp -> OpTy -> [CoreArg] -> CoreBndr
                -> Ghc.Type -> [CoreAlt]
                -> LocalEnv -> FreeVarsIndex
                -> KnownLocs -> Context x
                -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
transBinaryCase cond ty args bndr alt_ty alts@[_,_] env0 fvi locs0 ctxt = do
  -- TODO: We may want to get the result of the comparison as a
  -- Bool.  In the True branch we therefore want to have:
  --
  -- > bndr :-> loadLit True
  --
  (bcis, locs1, fvs, [r1, r2]) <- transArgs args env0 fvi locs0
--  let locs2 = updateLoc locs1 bndr (InVar r)
  let env = extendLocalEnv env0 bndr undefined
  let match_var = error "There must be no binders in comparison binops"
  let (trueBody, falseBody) =
        case alts of
          [(DEFAULT, [], b1), (DataAlt c, [], b2)]
           | c == Ghc.trueDataCon  -> (b2, b1)
           | c == Ghc.falseDataCon -> (b1, b2)
          [(DataAlt c1, [], b1), (DataAlt _, [], b2)]
           | c1 == Ghc.trueDataCon  -> (b1, b2)
           | c1 == Ghc.falseDataCon -> (b2, b1)
  -- If the context requires binding to a variable, then we have to
  -- make sure both branches write their result into the same
  -- variable.
  ctxt' <- (case ctxt of
             RetC -> return RetC
             BindC mr -> BindC . Just <$> mbFreshLocal alt_ty mr)
            :: Trans (Context x)

  let transUnaryConAlt body con_id = do
        let locs2 = updateLoc locs1 bndr (Global con_id)
        l <- freshLabel
        (bcis, _locs1, fvs1, _mb_var)
          <- transBody body env fvi locs2 ctxt'
        return (l, mkLabel l <*> bcis, fvs1)

  (tLabel, tBcis, tFvs) <- transUnaryConAlt trueBody trueDataConId
  (fLabel, fBcis, fFvs) <- transUnaryConAlt falseBody falseDataConId

  case ctxt' of
    RetC -> do
      return (bcis <*> insBranch cond ty r1 r2 tLabel fLabel
                   |*><*| tBcis |*><*| fBcis,
              locs1, mconcat [fvs, tFvs, fFvs], Nothing)
    BindC (Just r) -> do
      l <- freshLabel
      return (bcis <*> insBranch cond ty r1 r2 tLabel fLabel
                |*><*| tBcis <*> insGoto l
                |*><*| fBcis <*> insGoto l
                |*><*| mkLabel l,
              locs1, mconcat [fvs, tFvs, fFvs], Just r)

addMatchLocs :: KnownLocs -> BcVar -> AltCon -> [CoreBndr] -> KnownLocs
addMatchLocs locs _base_reg DEFAULT [] = locs
addMatchLocs locs _base_reg (LitAlt _) [] = locs
addMatchLocs locs base_reg (DataAlt _) vars =
  extendLocs locs [ (x, Field base_reg n t)
                  | (x,n) <- zip vars [1..]
                  , let t = repType (Ghc.varType x) ]

dataConTag :: AltCon -> BcTag
dataConTag DEFAULT = DefaultTag
dataConTag (DataAlt dcon) = Tag $ Ghc.dataConTag dcon
dataConTag (LitAlt (Ghc.MachInt n)) = LitT n

-- | Append a @Ret1@ instruction if needed and return.
maybeAddRet :: Context x -> Bcis O -> KnownLocs -> FreeVars -> BcVar
            -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
maybeAddRet (BindC _) is locs fvs r =
  return (is, locs, fvs, Just r)
maybeAddRet RetC is locs fvs r =
  return (is <*> insRet1 r, locs, fvs, Nothing)

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

isGhcConWrapId :: CoreBndr -> Bool
isGhcConWrapId x
  | Ghc.DataConWrapId _ <- Ghc.idDetails x = True
  | otherwise                              = False

ghcIdDataCon :: CoreBndr -> Ghc.DataCon
ghcIdDataCon x
  | Ghc.DataConWorkId dcon <- Ghc.idDetails x = dcon
  | otherwise = error "ghcIdDataCon: Id is not a DataConWorkId"

-- | Return @Just p@ iff input is a primitive operation.
isGhcPrimOpId :: CoreBndr -> Maybe Ghc.PrimOp
isGhcPrimOpId x
  | Ghc.PrimOpId p <- Ghc.idDetails x = Just p
  | otherwise                         = Nothing

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
    _ -> Nothing

primOpOther :: Ghc.PrimOp -> Maybe (PrimOp, [OpTy], OpTy)
primOpOther primop =
  case primop of
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

    _ -> Nothing

isCondPrimOp :: Ghc.PrimOp -> Maybe (BinOp, OpTy)
isCondPrimOp primop =
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

-- | View expression as n-ary application.  The expression in function
-- position must be a variable.  Ignores type abstraction, notes and
-- coercions.
--
-- > viewApp [| f @a x y 42# |] = Just (f, [x, y, 42#])
-- > viewApp [| case e of ... |] = Nothing
-- > viewApp [| 42# x |] = Nothing
--
-- The returned binder type has all type applications applied and
-- all type abstractions removed.  For example:
--
-- > -- full type of (>>=)
-- > Ghc.Base.>>= ::
-- >    forall (m :: * -> *). GHC.Base.Monad m =>
-- >      forall a b. m a -> (a -> m b) -> m b
--
-- For the expression:
-- 
-- > GHC.Base.>>= @ m $dMonad @ a @ b m sat_saP
--
-- this function will return
--
-- > Just (GHC.Base.>>=, [$dMonad, m, sat_saP]
--
-- and the type of the returned @GHC.Base.>>=@ will be
--
-- > GHC.Base.Monad m -> m a -> (a -> m b) -> m b
--
-- We use this to compute the representation type of the variable in
-- function position.
viewGhcApp :: CoreExpr -> Maybe (CoreBndr, [CoreArg])
viewGhcApp expr = {- tracePpr expr $ trace test1 $ go expr [] -}
   do (f, ctx, args) <- go1 expr []
      let !f_with_usage_type = adjustVarTy' f (Ghc.varType f) ctx
      let !real_args = filter (not . Ghc.isTypeArg) args
      return (f_with_usage_type, real_args)
 where
   go1 (Ghc.Var v)     as = Just (v, EmptyContext, as)
   go1 (Ghc.App f a)   as = do (f', ctx, as') <- go1 f (a:as)
                               return (f', AppCtx ctx a, as')
   go1 (Ghc.Tick _ e)  as = go1 e as
   go1 (Ghc.Cast e co) as = do (f', ctx, as') <- go1 e as
                               return (f', CastCtx ctx co, as')
   go1 (Ghc.Lam x e)   as | isTyVar x = go1 e as
   go1 _               _  = Nothing

   -- Annotate the binder with the usage type, not the (polymorphic)
   -- definition type.
   adjustVarTy' :: CoreBndr -> Ghc.Type -> AppContext -> CoreBndr
   adjustVarTy' v _ EmptyContext = v
   adjustVarTy' v ty ctx =
     let !ty' = typeInContext ty ctx in
     let !r = if Ghc.isGlobalId v then
               Ghc.mkGlobalVar (Ghc.idDetails v) (Ghc.varName v) ty' (Ghc.idInfo v)
              else
               Ghc.mkLocalVar (Ghc.idDetails v) (Ghc.varName v) ty' (Ghc.idInfo v)
     in r

data AppContext
  = EmptyContext                    -- []
  | AppCtx AppContext CoreArg       -- ([] {type}) -or- ([] arg)
  | CastCtx AppContext Ghc.Coercion -- ([] |> co)

typeInContext :: Ghc.Type -> AppContext -> Ghc.Type
typeInContext ty ctx =
  let (!res, !rebuild) = applyContext ty ctx in rebuild res

applyContext :: Ghc.Type -> AppContext -> (Ghc.Type, Ghc.Type -> Ghc.Type)
applyContext ty EmptyContext = (ty, id)
applyContext ty (AppCtx ctx (Type a)) =
  let (!ty', !rebuild) = applyContext ty ctx in
  case Ghc.splitForAllTy_maybe ty' of
    Just (_tyvar, _bodyType) -> (Ghc.applyTy ty' a, rebuild)
    Nothing -> error $ "Type arguments can only be applied to forall types: "
                  ++ showPpr (ty' :: Ghc.Type, (Type a) :: CoreArg)
applyContext ty (AppCtx ctx var) =
  let (!ty', !rebuild) = applyContext ty ctx in
  let !varTy = Ghc.exprType var in
  case Ghc.splitFunTy_maybe ty' of
    Just (!arg, !resTy) -> (resTy, \res ->
                                     let !res' = Ghc.mkFunTy arg res in
                                     rebuild res')
    Nothing -> error $ "Applying argument to type that's not a function type: "
                  ++ showPpr (ty', var)
applyContext ty (CastCtx ctx co) =
  let (!ty', !rebuild) = applyContext ty ctx in
  let Pair _ ty'' = Ghc.coercionKind co in
  (ty'', rebuild)


-- | View expression as n-ary abstraction.  Ignores type abstraction.
--
-- > viewGhcLam [| /\a b \x y -> exp |] = ([x, y], exp)
viewGhcLam :: CoreExpr -> ([CoreBndr], CoreExpr)
viewGhcLam expr = go expr []
 where
   go (Ghc.Lam x e) xs
     | isTyVar x = go e xs
     | otherwise = go e (x:xs)
   go (Cast e _) xs = go e xs
   go (Tick _ e) xs = go e xs
   go e xs = (reverse xs, e)


-- | Look through noise in arguments.  Ignores things like type
-- applications, coercions, type abstractions and notes.
--
-- Requires the @CorePrep@ invariants to hold.
viewGhcArg :: CoreArg -> Either Ghc.Literal Ghc.Id
viewGhcArg (Ghc.Var x)              = Right x
viewGhcArg (Ghc.Lit l)              = Left l
viewGhcArg (Ghc.App x (Ghc.Type _)) = viewGhcArg x
viewGhcArg (Lam a x) | isTyVar a    = viewGhcArg x
viewGhcArg (Cast x _)               = viewGhcArg x
viewGhcArg (Tick _ x)               = viewGhcArg x

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
