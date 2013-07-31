{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables, GADTs, GeneralizedNewtypeDeriving #-}
module Lambdachine.Grin.Analyse
  ( -- * Running Analyses
    M, runM,
    analyseAndRewriteBCOBwd,
    -- * Liveness Analysis
    livenessAnalysis2, LiveVars, live, insDefines,

    -- * Utils
    isVoid,
{-
    -- * Liveness Analysis with Symbolic Live Ranges
    SymLives(..), SymRange(..), symLivenessLattice,
-}  
  )
where

import Lambdachine.Ghc.Utils ( transType )
import Lambdachine.Grin.Bytecode
import Lambdachine.Utils.Pretty
import Lambdachine.Utils ( thd3 )

import qualified Data.Set as S
import qualified Data.Map as M
import Compiler.Hoopl hiding ( mkBRewrite, mkBRewrite3 )
import Data.Generics.Uniplate.Direct
import Data.Maybe ( fromMaybe )
import Data.Supply
import Debug.Trace

instance Show ChangeFlag where
  show SomeChange = "SomeChange"
  show NoChange = "NoChange"

-- TODO: For the transformation monad we have to make sure that:
--
--  a. Uniques are actually generated sequentially, so that we can
--     reliably implement the checkpointing mechanism.
--
--  b. The generated uniques won't overlap with existing uniques --
--     i.e., they should use a separate name space.

instance Pretty a => Pretty (LabelMap a) where
  ppr m = ppr (mapToList m)

type M a = SimpleUniqueMonad a -- SimpleFuelMonad

-- instance FuelMonad M where
--   getFuel = return 42
--   setFuel _ = return ()

runM :: M a -> a
runM m = runSimpleUniqueMonad m

-- | Run a backward pass on a 'BytecodeObject'.
analyseAndRewriteBCOBwd :: (CheckpointMonad m) =>
                           BytecodeObject
                        -> BwdPass m BcIns f
                        -> Fact C f  -- or just "f"?
                        -> m (BytecodeObject, FactBase f)
analyseAndRewriteBCOBwd bco@BcoCon{} _ _ = return (bco, noFacts)
analyseAndRewriteBCOBwd bco@BcObject{} pass exitfacts = do
  (g', f', _) <- analyzeAndRewriteBwd pass (NothingC :: MaybeC O Label)
                   (bcoCode bco) exitfacts
  return (bco{ bcoCode = g' }, f')

livenessAnalysis2 :: {- FuelMonad m => -} Monad m => BwdPass m BcIns LiveVars
livenessAnalysis2 = --debugBwdJoins trace (const True) $
  BwdPass { bp_lattice = livenessLattice
          , bp_transfer = liveness
          , bp_rewrite = voidAssignmentElim
          -- noBwdRewrite --deadAssignmentElim
          }

type LiveVars = S.Set BcVar

livenessLattice :: DataflowLattice LiveVars
livenessLattice =
  DataflowLattice
    { fact_name = "Live variables"
    , fact_bot = S.empty
    , fact_join = add }
 where
   add _lbl (OldFact old) (NewFact new) = (trace ("CHANGED:"++show changed) changed, joined)
     where
       joined = new `S.union` old
       changed = changeIf (S.size joined > S.size old)

liveness :: BwdTransfer BcIns LiveVars
liveness = mkBTransfer live

-- Fact is a type family: for O nodes there's only a unique
-- successor, so it's just LiveVars.  For C nodes there could be
-- arbitrarily many successors, so we have to use lookup.
live :: BcIns e x -> Fact x LiveVars -> LiveVars
live ins f = case ins of
  Label _     -> f
  Assign x r  -> addLives (S.delete x f) (universeBi r)
  Store b _ v -> addOne b (addOne v f)
  Eval l _ r  -> addOne r (fact f l)
  Goto l      -> fact f l
  Ret1 r      -> addOne r (fact_bot livenessLattice)
  RetN rs     -> addLives (fact_bot livenessLattice) rs
  CondBranch _ _ r1 r2 tl fl ->
    addLives (fact f tl `S.union` fact f fl) [r1, r2]
  Case _ r targets ->
    addOne r (S.unions (map (fact f . thd3) targets))
  Call Nothing fn args -> addLives S.empty (fn:args)
  Call (Just (r, l, _)) fn args ->
    addLives (S.delete r (fromMaybe S.empty (lookupFact l f)))
             (fn:args)
 where
   fact :: FactBase (S.Set a) -> Label -> S.Set a
   fact f l = fromMaybe S.empty (lookupFact l f)

   addLives :: LiveVars -> [BcVar] -> LiveVars
   addLives !l [] = l
   addLives !l (r:rs) --  isVoid r  = addLives l rs
                      | otherwise = addLives (S.insert r l) rs

   addOne :: BcVar -> LiveVars -> LiveVars
   addOne var lives --  isVoid var = lives
                    | otherwise  = S.insert var lives

insDefines :: BcIns e x -> S.Set BcVar
insDefines ins = case ins of
  Assign x _ -> S.singleton x
  Eval _ _ r -> S.singleton r
  Call (Just (r, _, _)) _ _ -> S.singleton r
  _ -> S.empty

insUses :: BcIns e x -> [BcVar]
insUses (Assign _ rhs)   = universeBi rhs
insUses (Eval _ _ x)     = [x]
insUses (Store _ _ x)    = [x]
insUses (Ret1 x)         = [x]
insUses (RetN xs)        = xs
insUses (CondBranch _ _ x y _ _) = [x, y]
insUses (Case _ x _)     = [x]
insUses (Call _ fn args) = fn : args
insUses _                = []

mkBRewrite3 :: forall n f m. Monad m =>
               (n C O -> f          -> m (Maybe (Graph n C O)))
            -> (n O O -> f          -> m (Maybe (Graph n O O)))
            -> (n O C -> FactBase f -> m (Maybe (Graph n O C)))
            -> BwdRewrite m n f
mkBRewrite3 f m l = BwdRewrite3 (lift f, lift m, lift l)
  where lift :: forall t t1 a. (t -> t1 -> m (Maybe a))
                             -> t -> t1 -> m (Maybe (a, BwdRewrite m n f))
        {-# INLINE lift #-}
        lift rw node fact = do
             a <- rw node fact
             case a of
               Nothing -> return Nothing
               Just a  -> return (Just (a,noBwdRewrite))

-- | Functions passed to 'mkBRewrite' should not be aware of the fuel supply.
-- The result returned by 'mkBRewrite' respects fuel.
mkBRewrite :: Monad m => (forall e x . n e x -> Fact x f -> m (Maybe (Graph n e x)))
           -> BwdRewrite m n f
mkBRewrite f = mkBRewrite3 f f f


voidAssignmentElim ::
--  forall m. FuelMonad m =>
    Monad m =>
    BwdRewrite m BcIns LiveVars
voidAssignmentElim = mkBRewrite rewrite
 where
   rewrite :: Monad m => BcIns e x -> Fact x LiveVars -> m (Maybe (Graph BcIns e x))
   rewrite (Assign dst (Move src)) _fact
     | isVoid dst = return (Just emptyGraph)
   rewrite _ins _fact = return Nothing

isVoid :: BcVar -> Bool
isVoid (BcVar _ ty) = transType ty == VoidTy
isVoid (BcReg _ ty) = ty == VoidTy

deadAssignmentElim :: 
  forall m. FuelMonad m => 
     BwdRewrite m BcIns LiveVars
  -- "forall" needed because because of local type signature below.
deadAssignmentElim = mkBRewrite rewrite
 where
   rewrite :: BcIns e x -> Fact x LiveVars -> m (Maybe (Graph BcIns e x))
   rewrite (Assign x _) lives
     | not (x `S.member` lives) = return $ Just emptyGraph
   rewrite _ _ = return Nothing

{-
-- | Inserts explicit 'Discard' instructions at the end of each live range.
insertDiscard :: forall m. FuelMonad m => BwdRewrite m BcIns LiveVars
--insertDiscard = mkBRewrite rewrite
insertDiscard = deepBwdRw rewrite
 where
   rewrite :: BcIns e x -> Fact x LiveVars -> m (Maybe (Graph BcIns e x))
   --rewrite _ _ | trace "moo" False = return undefined
   rewrite ins lives = case ins of
     Label l -> return Nothing -- TODO?
--     Discard x | x `S.member` lives -> return (Just GNil)
--               | otherwise        -> return Nothing
     CondBranch _ _ x y l1 l2 -> return Nothing -- TODO:
     Call _ fn args -> return Nothing -- TODO:
     Case _ x _     -> return Nothing -- TODO:
     Ret1 _         -> return Nothing -- implicit discard
     Assign _ _  -> rewriteOO ins lives
     Eval _      -> rewriteOO ins lives
     Store _ _ _ -> rewriteOO ins lives
     _ -> return Nothing

   rewriteOO :: BcIns O O -> Fact O LiveVars -> m (Maybe (Graph BcIns O O))
   rewriteOO ins lives
     | trace (pretty $ ppr ins <+> ppr (insUses ins) <+> ppr lives) False = undefined
   rewriteOO ins lives = do
     let discards = [ Discard x | x <- insUses ins,
                                  not (x `S.member` lives) ]
     trace (pretty $ text "DISCARDS:" <+> ppr discards) $
      if not (null discards) then
        trace "REPLACING" $ return (Just (mkMiddles (ins : discards)))
       else
        return Nothing
-}
