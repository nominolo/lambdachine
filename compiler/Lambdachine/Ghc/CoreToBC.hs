{-# LANGUAGE ViewPatterns #-}
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

import Lambdachine.Utils
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
import qualified DataCon as Ghc
import qualified CoreSyn as Ghc ( Expr(..) )
import qualified PrimOp as Ghc
import qualified TysWiredIn as Ghc ( trueDataConId, falseDataConId )
import Outputable ( Outputable, showPpr )
import CoreSyn ( CoreBind, CoreBndr, CoreExpr, CoreArg, CoreAlt,
                 Bind(..), Expr(Lam, Let, Type, Cast, Note),
                 AltCon(..),
                 collectBinders, flattenBinds, collectArgs )
import Var ( isTyVar )
import Unique ( Uniquable(..), getKey )

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative
import Control.Monad.State
--import Control.Monad.Fix
import Data.Foldable ( toList )
import Data.Monoid
import Data.Maybe ( fromMaybe )

import Debug.Trace

----------------------------------------------------------------------
-- Debug Utils:

unimplemented :: String -> a
unimplemented str = error $ "UNIMPLEMENTED: " ++ str

tracePpr :: Outputable a => a -> b -> b
tracePpr o exp = trace (">>> " ++ showPpr o) exp

-- -------------------------------------------------------------------
-- Top-level Interface

generateBytecode :: Supply Unique -> [CoreBind] -> BCOs
generateBytecode us bndrs0 = 
  runTrans us $ do toplevel_bcos <- go bndrs0 mempty
                   local_bcos <- getBCOs
                   return (M.union toplevel_bcos local_bcos)
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

transTopLevelBind :: CoreBndr -> CoreExpr -> Trans BCOs
transTopLevelBind f (viewGhcLam -> (params, body)) = do
  let !f' = toplevelId f
  let bco_type | (_:_) <- params = BcoFun (length params)
               | looksLikeCon body = Con
               | otherwise = CAF
  case bco_type of
    Con -> buildCon f' body
    _ -> do
      let env0 = mkLocalEnv [(x, undefined) | x <- params]
          locs0 = mkLocs [ (b, InReg n) | (b, n) <- zip params [0..] ]
          fvi0 = Ghc.emptyVarEnv
      (bcis, _, fvs, Nothing) <- transBody body env0 fvi0 locs0 RetC
      let bco = BcObject { bcoType = bco_type
                         , bcoCode = toList bcis
                         , bcoGlobalRefs = []
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
  let dcon' = toplevelId dcon
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
transBind :: CoreBind -- ^ The binding group to translate.
          -> LocalEnv -- ^ The 'LocalEnv' at the binding site.
          -> Trans [(Ghc.Id, ClosureInfo)]
transBind (NonRec x (viewGhcApp -> Just (f, args))) _env0
 | isGhcConWorkId f 
 = return [(x, ConObj f args)] --(transFields id args))]
 | otherwise
 = return [(x, AppObj f args)] -- (Right f : transFields id args))]
transBind (NonRec x (viewGhcLam -> (bndrs, body))) env0 = do
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
  let arity = length bndrs
  let bco = BcObject { bcoType = if arity > 0 then BcoFun arity else Thunk
                     , bcoCode = toList bcis
                     , bcoConstants = []
                     , bcoGlobalRefs = toList gbls
                     , bcoFreeVars = length vars }
  addBCO x' bco
  return [(x, FunObj arity x' vars)]

transBinds :: CoreBind -> LocalEnv -> FreeVarsIndex 
           -> KnownLocs
           -> Trans (Bcis, KnownLocs, FreeVars, LocalEnv)
transBinds bind env fvi locs0 = do
  obj_binds <- transBind bind env
  let env' = extendLocalEnvList env (map fst obj_binds)
  (bcis, locs, fvs) <- go mempty locs0 mempty obj_binds
  return (bcis, locs, fvs, env')
 where
   -- TODO: Do we need to accumulate the environment?
   go bcis locs fvs [] = return (bcis, locs, fvs)
   go bcis locs0 fvs ((x, ConObj dcon fields) : objs) = do
     (bcis1, locs1, fvs1, Just r)
       <- transStore dcon fields env fvi locs0 (BindC Nothing)
     let locs2 = updateLoc locs1 x (InVar r)
     go (bcis `mappend` bcis1) locs2 (fvs `mappend` fvs1) objs
   go bcis locs0 fvs ((x, AppObj f args) : objs) = do
     (bcis1, locs1, fvs1, (freg:regs))
       <- transArgs (Ghc.Var f : args) env fvi locs0
     rslt <- mbFreshLocal Nothing
     let bcis2 = (bcis `mappend` bcis1) `snocBag` MkAp rslt freg regs
         locs2 = updateLoc locs1 x (InVar rslt)
     go bcis2 locs2 (fvs `mappend` fvs1) objs
   go bcis locs0 fvs ((x, FunObj _arity info_tbl args) : objs) = do
     (bcis1, locs1, fvs1, regs)
       <- transArgs (map Ghc.Var args) env fvi locs0
     tag_reg <- mbFreshLocal Nothing
     rslt <- mbFreshLocal Nothing
     let bcis2 = 
           ((bcis `mappend` bcis1) `snocBag` LoadG tag_reg info_tbl)
             `snocBag` Store rslt tag_reg regs
         locs2 = updateLoc locs1 x (InVar rslt)
     go bcis2 locs2 (fvs `mappend` fvs1) objs

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

type BCOs = M.Map Id (BytecodeObject BcVar BcConst)
type BCO = BytecodeObject BcVar BcConst

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

addBCO :: Id -> BCO -> Trans ()
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


-- | A bytecode variable.
data BcVar = BcVar !Id
           | BcReg Int
  deriving (Eq, Ord)

instance Pretty BcVar where
  ppr (BcVar x) = ppr x
  ppr (BcReg n) = char 'R' <> ppr n

type Bci = BcInstr BcVar BcConst
type Bcis = Bag Bci

data Context = RetC | BindC (Maybe BcVar)

contextVar :: Context -> Maybe BcVar
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
  return (f (freshName us nm))

mbFreshLocal :: Maybe BcVar -> Trans BcVar
mbFreshLocal (Just v) = return v
mbFreshLocal Nothing = freshVar "lcl" (BcVar . mkLocalId)

--transBinder :: CoreExpr -> Trans BCO
{-
transRhs :: CoreExpr -> LocalEnv -> Trans Bcis
transRhs (viewGhcLam -> (bndrs, body)) env0 = do
  xs <- mapM internCoreBndr bndrs
  let env = fold2l' extendLocalEnv env0 bndrs xs
  let locs0 = mkLocs [ (b, InReg n) | (b, n) <- zip bndrs [0..] ]
  (bcis, _, _, _) <- transBody body env locs0 RetC
  return bcis
-}
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
  -> Context   -- ^ The code generation context.
  -> Trans (Bcis, KnownLocs, FreeVars, Maybe BcVar)
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
    RetC -> return (is `mappend` singletonBag (Ret1 r),
                   locs, mempty, Nothing)
    BindC _ -> return (is, locs, mempty, Just r)

transBody (Ghc.Var x) env fvi locs0 ctxt = do
  (is0, r, eval'd, locs1, fvs) <- transVar x env fvi locs0 (contextVar ctxt)
  let is | eval'd = is0
         | otherwise = is0 `mappend` singletonBag (Eval r r)
  case ctxt of
    RetC -> return (is `mappend` singletonBag (Ret1 r),
                   locs1, fvs, Nothing)
    _ -> return (is, locs1, fvs, Just r)

transBody expr@(Ghc.App _ _) env fvi locs0 ctxt
 | Just (f, args) <- viewGhcApp expr
 = transApp f args env fvi locs0 ctxt
   
transBody (Ghc.Case scrut bndr _ty alts) env0 fvi locs0 ctxt = do
  (bcis, locs1, fvs0, Just r) <- transBody scrut env0 fvi locs0 (BindC Nothing)
  let locs2 = updateLoc locs1 bndr (InVar r)
  let env = extendLocalEnv env0 bndr undefined
  ctxt' <- case ctxt of
             BindC Nothing -> BindC . Just <$> mbFreshLocal Nothing
             _ -> return ctxt
  case alts of
    [(altcon, vars, body)] -> do
      let locs3 = addMatchLocs locs2 r altcon vars
          env' = extendLocalEnvList env vars
      (bcis', locs4, fvs1, mb_r) <- transBody body env' fvi locs3 ctxt
      return (bcis `mappend` bcis', locs4, fvs0 `mappend` fvs1, mb_r)
    _ -> do
      (alt_bcis, fvs1) <- transCaseAlts alts r env fvi locs2 ctxt'
      return (bcis `snocBag` Case r (map (second toList) alt_bcis),
              locs1, fvs0 `mappend` fvs1, contextVar ctxt')

transBody (Ghc.Let (NonRec x (viewGhcApp -> Just (f, args@(_:_)))) body)
          env fvi locs0 ctxt
 | isGhcConWorkId f
  = do (bcis0, locs1, fvs0, Just r)
         <- transStore f args env fvi locs0 (BindC Nothing)
       let locs2 = updateLoc locs1 x (InVar r)
           env' = extendLocalEnv env x undefined
       (bcis1, locs3, fvs1, mb_r) <- transBody body env' fvi locs2 ctxt
       return (bcis0 `mappend` bcis1, locs3, fvs0 `mappend` fvs1, mb_r)

transBody (Ghc.Let bind body) env fvi locs0 ctxt = do
  (bcis, locs1, fvs, env') <- transBinds bind env fvi locs0
  (bcis', locs2, fvs', mb_r) <- transBody body env' fvi locs1 ctxt
  return (bcis `mappend` bcis', locs2, fvs `mappend` fvs', mb_r)

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
             -> Trans (Bcis, BcVar)
transLiteral (Ghc.MachInt n) mbvar = do
  rslt <- mbFreshLocal mbvar
  return (singletonBag (LoadK rslt (CInt n)), rslt)

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
  -> Trans (Bcis, BcVar, Bool, KnownLocs, FreeVars)
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
--transVar _ _ _ _ _ | trace "transVar" False = undefined
transVar x env fvi locs0 mr =
  case lookupLoc locs0 x of
    Just (InVar x') -> -- trace "inVAR" $
      return (mbMove mr x', fromMaybe x' mr, False, locs0, mempty)
    Just (InReg r) -> -- trace "inREG" $
      let x' = BcReg r in
      return (mbMove mr x', fromMaybe x' mr, False, locs0, mempty)
    Just (Field p n) -> do -- trace "inFLD" $ do
      r <- mbFreshLocal mr
      return (singletonBag (Fetch r p n),
              r, False, updateLoc locs0 x (InVar r), mempty)
    Nothing
      | Just x' <- lookupLocalEnv env x -> do
          -- Note: To avoid keeping track of two environments we must
          -- only reach this case if the variable is bound outside the
          -- current closure.
          r <- mbFreshLocal mr
          -- Do not force @i@ -- must remain a thunk
          let i = expectJust "transVar" (Ghc.lookupVarEnv fvi x)
          return (singletonBag (LoadF r i), r, False,
                  updateLoc locs0 x (InVar r), closureVar x)
      | otherwise -> do  -- global variable
          let x' = toplevelId x
          r <- mbFreshLocal mr
          return (singletonBag (LoadG r x'), r, isGhcConWorkId x,  -- TODO: only if CAF
                  updateLoc locs0 x (InVar r), globalVar x')

transApp :: CoreBndr -> [CoreArg] -> LocalEnv -> FreeVarsIndex
         -> KnownLocs -> Context
         -> Trans (Bcis, KnownLocs, FreeVars, Maybe BcVar)
transApp f [] env fvi locs ctxt = transBody (Ghc.Var f) env fvi locs ctxt
transApp f args env fvi locs0 ctxt
  | Just p <- isGhcPrimOpId f, isLength 2 args 
  = do (is0, locs1, fvs, [r1, r2]) <- transArgs args env fvi locs0
       let Just (op, ty) = primOpToBinOp p   -- XXX: may fail
       rslt <- mbFreshLocal (contextVar ctxt)
       maybeAddRet ctxt (is0 `snocBag` BinR op ty rslt r1 r2)
                   locs1 fvs rslt
  | isGhcConWorkId f  -- allocation
  = transStore f args env fvi locs0 ctxt
  | otherwise
  = do (is0, locs1, fvs0, regs) <- transArgs args env fvi locs0
       (is1, locs2, fvs1, Just fr)
         <- transBody (Ghc.Var f) env fvi locs1 (BindC Nothing)
       let is = is0 `mappend` is1
           fvs = fvs0 `mappend` fvs1
           mb_rslt = contextVar ctxt
       return (is `snocBag` Call mb_rslt fr regs,
               locs2, fvs, mb_rslt)

--error $ "transApp: " ++ showPpr f ++ " " ++ showPpr (Ghc.idDetails f)

-- | Generate code for loading the given function arguments into
-- registers.
transArgs :: [CoreArg] -> LocalEnv -> FreeVarsIndex -> KnownLocs
          -> Trans (Bcis, KnownLocs, FreeVars, [BcVar])
transArgs args0 env fvi locs0 = go args0 mempty locs0 mempty []
 where
   go [] bcis locs fvs regs = return (bcis, locs, fvs, reverse regs)
   go (arg:args) bcis locs fvs regs = do
     (bcis', locs', fvs', r) <- trans_arg arg locs
     go args (bcis `mappend` bcis') locs' (fvs `mappend` fvs') (r:regs)

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
           -> KnownLocs -> Context
           -> Trans (Bcis, KnownLocs, FreeVars, Maybe BcVar)
transStore dcon args env fvi locs0 ctxt = do
  (bcis0, locs1, fvs, regs) <- transArgs args env fvi locs0
  (bcis1, con_reg, _, locs2, fvs')
    <- transVar dcon env fvi locs1 (contextVar ctxt)  -- XXX: loadDataCon or sth.
  rslt <- mbFreshLocal (contextVar ctxt)
  let bcis = (bcis0 `mappend` bcis1) `snocBag` Store rslt con_reg regs
  maybeAddRet ctxt bcis locs2 (fvs `mappend` fvs') rslt

transCaseAlts ::
     [CoreAlt] -- ^ The case alternatives
  -> BcVar     -- ^ The variable we're matching on.
  -> LocalEnv -> FreeVarsIndex -> KnownLocs -> Context
  -> Trans ([(BcTag, Bcis)], FreeVars)
transCaseAlts alts match_var env fvi locs0 ctxt = do
  second mconcat . unzip <$>
    (forM alts $ \(altcon, vars, body) -> do
      let locs1 = addMatchLocs locs0 match_var altcon vars
          env' = extendLocalEnvList env vars
      (bcis, _locs2, fvs, _mb_var) <- transBody body env' fvi locs1 ctxt
      return ((dataConTag altcon, bcis), fvs))

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
maybeAddRet :: Context -> Bcis -> KnownLocs -> FreeVars -> BcVar
            -> Trans (Bcis, KnownLocs, FreeVars, Maybe BcVar)
maybeAddRet (BindC _) is locs fvs r =
  return (is, locs, fvs, Just r)
maybeAddRet RetC is locs fvs r =
  return (is `snocBag` Ret1 r, locs, fvs, Nothing)

-- | Return a move instruction if target is @Just x@.
--
-- Redundant move instructions are eliminated by a later pass.
mbMove :: Maybe BcVar -> BcVar -> Bcis
mbMove Nothing _ = mempty
mbMove (Just r) r'
  | r == r'   = mempty
  | otherwise = singletonBag (Move r r')

isGhcConWorkId :: CoreBndr -> Bool
isGhcConWorkId x
  | Ghc.DataConWorkId _ <- Ghc.idDetails x = True
  | otherwise                              = False

-- | Return @Just p@ iff input is a primitive operation.
isGhcPrimOpId :: CoreBndr -> Maybe Ghc.PrimOp
isGhcPrimOpId x
  | Ghc.PrimOpId p <- Ghc.idDetails x = Just p
  | otherwise                         = Nothing

-- TODO: This needs more thought.
primOpToBinOp :: Ghc.PrimOp -> Maybe (BinOp, OpTy)
primOpToBinOp primop =
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
--primOpToBinOp _ = Nothing


-- | Directly turn GHC 'Ghc.Id' into 'Id'.
--
-- Reuses the 'Unique' from GHC.
toplevelId :: Ghc.Id -> Id
toplevelId x = --  | Ghc.VanillaId <- Ghc.idDetails x =
  mkTopLevelId (N.mkBuiltinName (fromGhcUnique x) (Ghc.getOccString x))

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
   go e xs = (reverse xs, e)

