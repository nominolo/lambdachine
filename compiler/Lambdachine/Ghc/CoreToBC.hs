{-# LANGUAGE ViewPatterns, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving #-}
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

import Lambdachine.Utils hiding ( Uniquable(..) )
import Lambdachine.Id as N
import Lambdachine.Grin.Bytecode as Grin
import Lambdachine.Builtin
import Lambdachine.Utils.Unique ( mkBuiltinUnique )

import qualified Var as Ghc
import qualified VarEnv as Ghc
import qualified VarSet as Ghc
import qualified HscTypes as Ghc ( CoreModule(..) )
import qualified Module as Ghc
import qualified Literal as Ghc
import qualified Name as Ghc
import qualified IdInfo as Ghc
import qualified Id as Ghc
import qualified Type as Ghc
import qualified DataCon as Ghc
import qualified CoreSyn as Ghc ( Expr(..) )
import qualified PrimOp as Ghc
import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId )
import qualified TyCon as Ghc
import TyCon ( TyCon )
import Outputable ( Outputable, showPpr )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr, CoreArg, CoreAlt,
                 Bind(..), Expr(Lam, Let, Type, Cast, Note),
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
import Data.List ( foldl' )
import Data.Monoid
import Data.Maybe ( fromMaybe )

import Debug.Trace

----------------------------------------------------------------------
-- * Debug Utils:

unimplemented :: String -> a
unimplemented str = error $ "UNIMPLEMENTED: " ++ str

tracePpr :: Outputable a => a -> b -> b
tracePpr o exp = trace (">>> " ++ showPpr o) exp

-- -------------------------------------------------------------------
-- * Top-level Interface
type Bcis x = BcGraph O x

generateBytecode :: Supply Unique -> [CoreBind] -> [TyCon] -> BCOs
generateBytecode us bndrs0 data_tycons =
  runTrans us $ do
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

transTyCon :: TyCon -> BCOs
transTyCon tycon = do
  collect' M.empty (Ghc.tyConDataCons tycon) $ \bcos dcon ->
    let dcon_id = dataConInfoTableId dcon
        bco = BcConInfo { bcoConTag = Ghc.dataConTag dcon }
    in M.insert dcon_id bco bcos

transTopLevelBind :: CoreBndr -> CoreExpr -> Trans BCOs
transTopLevelBind f (viewGhcLam -> (params, body)) = do
  let !f' = toplevelId f
  let bco_type | (_:_) <- params = BcoFun (length params)
               | looksLikeCon body = Con
               | isGhcConWorkId f = Con
               | otherwise = CAF
  case bco_type of
    Con -> buildCon f' body
    _ -> do
      let env0 = mkLocalEnv [(x, undefined) | x <- params]
          locs0 = mkLocs [ (b, InReg n) | (b, n) <- zip params [0..] ]
          fvi0 = Ghc.emptyVarEnv
      (bcis, _, fvs, Nothing) <- transBody body env0 fvi0 locs0 RetC
      g <- finaliseBcGraph bcis
      let bco = BcObject { bcoType = bco_type
                         , bcoCode = g
                         , bcoGlobalRefs = toList (globalVars fvs)
                         , bcoConstants = []
                         , bcoFreeVars = 0
                         }
      return (M.singleton f' bco)

looksLikeCon :: CoreExpr -> Bool
looksLikeCon (viewGhcApp -> Just (f, args)) = 
  not (null args) && isGhcConWorkId f 
looksLikeCon _ = False

buildCon :: Id -> CoreExpr -> Trans BCOs
buildCon f (viewGhcApp -> Just (dcon, args0)) = do
  let dcon' = dataConInfoTableId (ghcIdDataCon dcon)
      fields = transFields toplevelId args0
  return (M.singleton f (BcoCon Con dcon' fields))

data ClosureInfo
  = ConObj Ghc.Id [CoreArg] --[Either BcConst Ghc.Id]
  | AppObj Ghc.Id [CoreArg] --[Either BcConst Ghc.Id]
  | FunObj !Int Id [Ghc.Id]

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
             FunObj _ _ frees ->
               [ (x, offs) | (offs, x) <- zip [1..] frees
                           , x `Ghc.elemVarSet` fwds ]
     let fix_acc' = foldl' (\e (x, offs) ->
                              Ghc.extendVarEnv_C (++) e x [(offs, f)])
                      fix_acc fwd_refs
     let fwds' = Ghc.delVarSet fwds f
     trans_binds_rec fwds' binds env fix_acc'
                     ((f, closure_info) : ci_acc)

-- Creates the actual code sequence (including fixup of forward references).
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
   go bcis locs0 fvs ((x, ConObj dcon fields) : objs) = do
     (bcis1, locs1, fvs1, Just r)
       <- transStore dcon fields env fvi locs0 (BindC Nothing)
     let locs2 = updateLoc locs1 x (InVar r)
     go (bcis <*> bcis1 <*> add_fw_refs x r locs2) locs2 (fvs `mappend` fvs1) objs
   go bcis locs0 fvs ((x, AppObj f args) : objs) = do
     (bcis1, locs1, fvs1, (freg:regs))
       <- transArgs (Ghc.Var f : args) env fvi locs0
     rslt <- mbFreshLocal Nothing
     let bcis2 = (bcis <*> bcis1) <*> insMkAp rslt (freg:regs)
         locs2 = updateLoc locs1 x (InVar rslt)
         bcis3 = bcis2 <*> add_fw_refs x rslt locs2
     go bcis3 locs2 (fvs `mappend` fvs1) objs
   go bcis locs0 fvs ((x, FunObj _arity info_tbl args) : objs) = do
     (bcis1, locs1, fvs1, regs)
       <- transArgs (map Ghc.Var args) env fvi locs0
     tag_reg <- mbFreshLocal Nothing
     rslt <- mbFreshLocal Nothing
     let bcis2 = bcis <*> bcis1 <*> insLoadGbl tag_reg info_tbl <*>
                 insAlloc rslt tag_reg regs
         locs2 = updateLoc locs1 x (InVar rslt)
         bcis3 = bcis2 <*> add_fw_refs x rslt locs2
     go bcis3 locs2 (fvs `mappend` fvs1) objs

   add_fw_refs x r locs =
     case Ghc.lookupVarEnv fwd_env x of
       Nothing -> emptyGraph --emptyBag
       Just fixups ->
         catGraphs $   -- listToBag $
           [ insStore ry n r | (n, y) <- fixups,
                               let Just (InVar ry) = lookupLoc locs y ]

transBind :: CoreBndr -- ^ The binder name ...
          -> CoreExpr -- ^ ... and its body.
          -> LocalEnv -- ^ The 'LocalEnv' at the binding site.
          -> Trans ClosureInfo
transBind x (viewGhcApp -> Just (f, args)) _env0
 | isGhcConWorkId f
 = return (ConObj f args)
 | otherwise
 = return (AppObj f args)
transBind x (viewGhcLam -> (bndrs, body)) env0 = do
  let locs0 = mkLocs [ (b, InReg n) | (b, n) <- zip bndrs [0..] ]
      env = fold2l' extendLocalEnv env0 bndrs (repeat undefined)

  -- Here comes the magic:
  (bcis, vars, gbls, _) <- mfix $ \ ~(_bcis, _vars, _gbls, fvis) -> do
    (bcis, locs', fvs, Nothing) <- transBody body env fvis locs0 RetC
    let closure_vars = Ghc.varSetElems (closureVars fvs)
        -- maps from closure variable to its index
        cv_indices = Ghc.mkVarEnv (zip closure_vars [(1::Int)..])
    return (bcis, closure_vars, globalVars fvs, cv_indices)
  x' <- freshVar "closure" mkTopLevelId
  g <- finaliseBcGraph bcis
  let arity = length bndrs
  let bco = BcObject { bcoType = if arity > 0 then BcoFun arity else Thunk
                     , bcoCode = g
                     , bcoConstants = []
                     , bcoGlobalRefs = toList gbls
                     , bcoFreeVars = length vars }
  addBCO x' bco
  return (FunObj arity x' vars)

transFields :: (Ghc.Id -> a) -> [CoreArg] -> [Either BcConst a]
transFields f args = map to_field args
 where
   to_field (Ghc.Lit (Ghc.MachInt n)) = Left (CInt n)
   to_field (Ghc.Var x)               = Right (f x)
   to_field (Ghc.App x (Ghc.Type _))  = to_field x
   to_field (Lam a x) | isTyVar a     = to_field x
   to_field (Cast x _)                = to_field x
   to_field (Note _ x)                = to_field x
   to_field arg = 
     error $ "transFields: Ill-formed argument: " ++ showPpr arg

-- -------------------------------------------------------------------

newtype Trans a = Trans (State TransState a)
  deriving (Functor, Applicative, Monad, MonadFix)

-- transFix :: (a -> Trans a) -> Trans a
-- transFix f = let Trans s = 

data TransState = TransState
  { tsUniques :: Supply Unique
  , tsLocalBCOs :: BCOs }

runTrans :: Supply Unique -> Trans a -> a
runTrans us (Trans m) = evalState m s0
 where
   s0 = TransState { tsUniques = us
                   , tsLocalBCOs = M.empty }

genUnique :: Trans (Supply Unique)
genUnique = Trans $ do
  s <- get
  case split2 (tsUniques s) of
    (us, us') -> do
      put $! s{ tsUniques = us }
      return us'

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
  | Field BcVar Int
    -- ^ The value can be loaded from memory by loading the nth slot
    -- from the given variable.
  | InReg Int
    -- ^ The value is in a specific register.
  | FreeVar Int
  | Fwd
    -- ^ A forward reference.
  | Global Id

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
newtype KnownLocs = KnownLocs (Ghc.IdEnv ValueLocation)

lookupLoc :: KnownLocs -> CoreBndr -> Maybe ValueLocation
lookupLoc (KnownLocs env) x = Ghc.lookupVarEnv env x

updateLoc :: KnownLocs -> CoreBndr -> ValueLocation -> KnownLocs
updateLoc (KnownLocs env) x l = KnownLocs $ Ghc.extendVarEnv env x l

extendLocs :: KnownLocs -> [(CoreBndr, ValueLocation)] -> KnownLocs
extendLocs (KnownLocs env) xls = 
  KnownLocs $ Ghc.extendVarEnvList env xls

noLocs :: KnownLocs
noLocs = KnownLocs Ghc.emptyVarEnv

mkLocs :: [(Ghc.Id, ValueLocation)] -> KnownLocs
mkLocs l = KnownLocs (Ghc.mkVarEnv l)

instance Monoid KnownLocs where
  mempty = noLocs
  (KnownLocs e1) `mappend` (KnownLocs e2) =
    KnownLocs (Ghc.plusVarEnv e1 e2)

-- | Keeps track of non-toplevel variables bound outside the current
-- bytecode context.  Consider the following example:
--
-- > f l y = case l of
-- >           Cons x xs -> let g = <body> in
-- >                        ...
--
-- Assume that @<body>@ mentions @y@ and @x@; these have to become
-- closure variables.  The bytecode for @g@ will look something like
-- this.
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

mbFreshLocal :: Maybe BcVar -> Trans BcVar
mbFreshLocal (Just v) = return v
mbFreshLocal Nothing = freshVar "%" (BcVar . mkLocalId)

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
   
transBody (Ghc.Case scrut bndr _ty alts) env0 fvi locs0 ctxt = do
  (bcis, locs1, fvs0, Just r) <- transBody scrut env0 fvi locs0 (BindC Nothing)
  let locs2 = updateLoc locs1 bndr (InVar r)
  let env = extendLocalEnv env0 bndr undefined
  case alts of
    [(altcon, vars, body)] -> do
      let locs3 = addMatchLocs locs2 r altcon vars
          env' = extendLocalEnvList env vars
      (bcis', locs4, fvs1, mb_r) <- transBody body env' fvi locs3 ctxt
      return (bcis <*> bcis', locs4, fvs0 `mappend` fvs1, mb_r)
    _ -> do
      case ctxt of
        RetC -> do -- inss are closed at exit
          (alts, inss, fvs1) <- transCaseAlts alts r env fvi locs2 RetC
          return ((bcis <*> insCase CaseOnTag {- XXX: wrong -} r alts)
                  `catGraphsC` inss,
                  locs1, fvs0 `mappend` fvs1, Nothing)
        BindC mr -> do -- close inss' first
          r <- mbFreshLocal mr
          (alts, inss, fvs1) <- transCaseAlts alts r env fvi locs2 (BindC (Just r))
          let bcis' =
                withFresh $ \l -> 
                  let inss' = [ ins <*> insGoto l | ins <- inss ] in
                  ((bcis <*> insCase CaseOnTag r alts) `catGraphsC` inss')
                   |*><*| mkLabel l  -- make sure we're open at the end
          return (bcis', locs1, fvs0 `mappend` fvs1, Just r)

transBody (Ghc.Let (NonRec x (viewGhcApp -> Just (f, args@(_:_)))) body)
          env fvi locs0 ctxt
 | isGhcConWorkId f
  = do (bcis0, locs1, fvs0, Just r)
         <- transStore f args env fvi locs0 (BindC Nothing)
       let locs2 = updateLoc locs1 x (InVar r)
           env' = extendLocalEnv env x undefined
       (bcis1, locs3, fvs1, mb_r) <- transBody body env' fvi locs2 ctxt
       return (bcis0 <*> bcis1, locs3, fvs0 `mappend` fvs1, mb_r)

transBody (Ghc.Let bind body) env fvi locs0 ctxt = do
  (bcis, locs1, fvs, env') <- transBinds bind env fvi locs0
  (bcis', locs2, fvs', mb_r) <- transBody body env' fvi locs1 ctxt
  return (bcis <*> bcis', locs2, fvs `mappend` fvs', mb_r)

transBody (Ghc.Note _ e) env fvi locs ctxt = transBody e env fvi locs ctxt
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
transLiteral (Ghc.MachInt n) mbvar = do
  rslt <- mbFreshLocal mbvar
  return (insLoadLit rslt (CInt n), rslt)
transLiteral (Ghc.MachStr fs) mb_var = do
  rslt <- mbFreshLocal mb_var
  return (insLoadLit rslt (CStr (unpackFS fs)), rslt)
transLiteral x _ = unimplemented ("literal: " ++ showPpr x)

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
--transVar x _ _ _ _ | trace ("transVar: " ++ showPpr x ++ " : " ++ showPpr (Ghc.idType x) ++ " / " ++ show (not (Ghc.isUnLiftedType (Ghc.idType x)))) False = undefined
transVar x env fvi locs0 mr =
  case lookupLoc locs0 x of
    Just (InVar x') -> -- trace "inVAR" $
      return (mbMove mr x', fromMaybe x' mr, in_whnf, locs0, mempty)
    Just (InReg r) -> -- trace "inREG" $
      let x' = BcReg r in
      return (mbMove mr x', fromMaybe x' mr, in_whnf, locs0, mempty)
    Just (Field p n) -> do -- trace "inFLD" $ do
      r <- mbFreshLocal mr
      return (insFetch r p n,
              r, in_whnf, updateLoc locs0 x (InVar r), mempty)
    Just Fwd -> do
      r <- mbFreshLocal mr
      return (insLoadBlackhole r, r, True, locs0, mempty)
    Nothing
      | Just x' <- lookupLocalEnv env x -> do
          -- Note: To avoid keeping track of two environments we must
          -- only reach this case if the variable is bound outside the
          -- current closure.
          r <- mbFreshLocal mr
          -- Do not force @i@ -- must remain a thunk
          let i = expectJust "transVar" (Ghc.lookupVarEnv fvi x)
          return (insLoadFV r i, r, in_whnf,
                  updateLoc locs0 x (InVar r), closureVar x)

      | otherwise -> do  -- global variable
          let x' | isGhcConWorkId x,
                   not (Ghc.isNullarySrcDataCon (ghcIdDataCon x))
                 = dataConInfoTableId (ghcIdDataCon x)
                 | otherwise
                 = toplevelId x
          r <- mbFreshLocal mr
          return (insLoadGbl r x', r, isGhcConWorkId x,  -- TODO: only if CAF
                  updateLoc locs0 x (InVar r), globalVar x')
 where
   in_whnf = Ghc.isUnLiftedType (Ghc.idType x)

transApp :: CoreBndr -> [CoreArg] -> LocalEnv -> FreeVarsIndex
         -> KnownLocs -> Context x
         -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
transApp f [] env fvi locs ctxt = transBody (Ghc.Var f) env fvi locs ctxt
transApp f args env fvi locs0 ctxt
  | Just p <- isGhcPrimOpId f, isLength 2 args 
  = do (is0, locs1, fvs, [r1, r2]) <- transArgs args env fvi locs0
       let Just (op, ty) = primOpToBinOp p   -- XXX: may fail
       rslt <- mbFreshLocal (contextVar ctxt)
       maybeAddRet ctxt (is0 <*> insBinOp op ty rslt r1 r2)
                   locs1 fvs rslt
  | isGhcConWorkId f  -- allocation
  = transStore f args env fvi locs0 ctxt
  | otherwise
  = do (is0, locs1, fvs0, regs) <- transArgs args env fvi locs0
       (is1, fr, _, locs2, fvs1)
          <- transVar f env fvi locs1 Nothing
       let is = is0 <*> is1
           fvs = fvs0 `mappend` fvs1
       case ctxt of
         RetC -> -- tailcall, x = C
           let ins = is <*> insCall Nothing fr regs in
           return (ins, locs2, fvs, Nothing)
         BindC mr ->  do
           -- need to ensure that x = O, so we need to emit
           -- a fresh label after the call
           r <- mbFreshLocal mr
           let ins = withFresh $ \l ->
                       is <*> insCall (Just (r, l)) fr regs |*><*| mkLabel l
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
   trans_arg (Note _ x) locs               = trans_arg x locs

transStore :: CoreBndr -> [CoreArg] -> LocalEnv -> FreeVarsIndex
           -> KnownLocs -> Context x
           -> Trans (Bcis x, KnownLocs, FreeVars, Maybe BcVar)
transStore dcon args env fvi locs0 ctxt = do
  (bcis0, locs1, fvs, regs) <- transArgs args env fvi locs0
  (bcis1, con_reg, _, locs2, fvs')
    <- transVar dcon env fvi locs1 (contextVar ctxt)  -- XXX: loadDataCon or sth.
  rslt <- mbFreshLocal (contextVar ctxt)
  let bcis = (bcis0 <*> bcis1) <*> insAlloc rslt con_reg regs
  maybeAddRet ctxt bcis locs2 (fvs `mappend` fvs') rslt

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

addMatchLocs :: KnownLocs -> BcVar -> AltCon -> [CoreBndr] -> KnownLocs
addMatchLocs locs _base_reg DEFAULT [] = locs
addMatchLocs locs _base_reg (LitAlt _) [] = locs
addMatchLocs locs base_reg (DataAlt _) vars =
  extendLocs locs [ (x, Field base_reg n) | (x,n) <- zip vars [1..] ]

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
    Ghc.IntAddOp -> Just (OpAdd, Int32Ty)
    Ghc.IntSubOp -> Just (OpSub, Int32Ty)
    Ghc.IntMulOp -> Just (OpMul, Int32Ty)
    Ghc.IntQuotOp -> Just (OpDiv, Int32Ty)

    -- TODO: treat conditionals specially?
    Ghc.IntGtOp -> Just (CmpGt, Int32Ty)
    Ghc.IntGeOp -> Just (CmpGe, Int32Ty)
    Ghc.IntEqOp -> Just (CmpEq, Int32Ty)
    Ghc.IntNeOp -> Just (CmpNe, Int32Ty)
    Ghc.IntLtOp -> Just (CmpLt, Int32Ty)
    Ghc.IntLeOp -> Just (CmpLe, Int32Ty)

    _ -> Nothing
--primOpToBinOp _ = Nothing


-- | Directly turn GHC 'Ghc.Id' into 'Id'.
--
-- Reuses the 'Unique' from GHC.
toplevelId :: Ghc.Id -> Id
toplevelId x = --  | Ghc.VanillaId <- Ghc.idDetails x =
  mkTopLevelId (N.mkBuiltinName (fromGhcUnique x) (Ghc.getOccString x))

dataConInfoTableId :: Ghc.DataCon -> Id
dataConInfoTableId dcon =
  mkDataConInfoTableId $
   N.mkBuiltinName (fromGhcUnique dcon) (Ghc.getOccString dcon)

-- | Take a GHC 'Unique.Unique' and turn it into a 'Unique'.
--
-- Be very careful when using this and make sure that the namespace
-- (the 'Char' argument to 'newUniqueSupply' and GHC's equivalent)
-- cannot possibly overlap.
fromGhcUnique :: Uniquable a => a -> Unique
fromGhcUnique x = fromExternalUnique (getKey (getUnique x))

-- | View expression as n-ary application.  The expression in function
-- position must be a variable.  Ignores type abstraction, notes and
-- coercions.
--
-- > viewApp [| f @a x y 42# |] = Just (f, [x, y, 42#])
-- > viewApp [| case e of ... |] = Nothing
-- > viewApp [| 42# x |] = Nothing
-- 
viewGhcApp :: CoreExpr -> Maybe (CoreBndr, [CoreArg])
viewGhcApp expr = go expr []
 where
   go (Ghc.Var v)          as = Just (v, as)
   go (Ghc.App f (Type _)) as = go f as
   go (Ghc.App f a)        as = go f (a:as)
   go (Ghc.Note _ e)       as = go e as
   go (Ghc.Cast e _)       as = go e as
   go (Ghc.Lam x e) as | isTyVar x = go e as
   go _ _ = Nothing

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
   go (Note _ e) xs = go e xs
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
viewGhcArg (Note _ x)               = viewGhcArg x
