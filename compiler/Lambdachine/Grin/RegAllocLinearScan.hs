{-# LANGUAGE BangPatterns, GADTs, GeneralizedNewtypeDeriving  #-}
module Lambdachine.Grin.RegAllocLinearScan 
  ( allocRegs )
where

import Lambdachine.Id ( Id )
import Lambdachine.Ghc.Utils
import Lambdachine.Grin.Bytecode hiding ( (<*>) )
import Lambdachine.Grin.Analyse
import Lambdachine.Utils

import Compiler.Hoopl hiding ( UniqueSet, UniqueMap, (<*>) )
import Data.Vector ( Vector )
import Data.Maybe ( fromMaybe )
import Control.DeepSeq
import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Applicative
import Data.Generics.Uniplate.Operations

import Debug.Trace

allocRegs :: String -> [String] -> BCOs
          -> BytecodeModule
allocRegs module_name module_imports bcos0 =
  let !bcos = M.mapWithKey allocRegsBco bcos0 in
  BytecodeModule
    { bcm_name = module_name
    , bcm_imports = module_imports
    , bcm_bcos = bcos
    }

allocRegsBco :: Id -> BytecodeObject -> BytecodeObject' FinalCode
allocRegsBco _name bco@BcoCon{} = -- this is just silly
  BcoCon{ bcoType = bcoType bco
        , bcoDataCon = bcoDataCon bco
        , bcoFields = bcoFields bco }
allocRegsBco _name bco@BcConInfo{} =
  BcConInfo{ bcoConTag = bcoConTag bco
           , bcoConFields = bcoConFields bco
           , bcoConArgTypes = bcoConArgTypes bco }
allocRegsBco _name bco@BcTyConInfo{} =
  BcTyConInfo{ bcoDataCons = bcoDataCons bco }
allocRegsBco name bco0@BcObject{} =
  BcObject{ bcoType = bcoType bco
          , bcoCode = code'
          , bcoGlobalRefs = bcoGlobalRefs bco
          , bcoConstants = bcoConstants bco
          , bcoFreeVars = bcoFreeVars bco }
 where
   (bco, live_facts) =
     runM $ analyseAndRewriteBCOBwd
              ({- trace ("INPUT_GRAPH:"++pretty name++"\n\n"++pretty (bcoCode bco0)) -} bco0)
              livenessAnalysis2 noFacts
   !code' = allocRegsGraph (bcoArity bco0) live_facts (bcoCode bco)

data LinearCode = LinearCode !(Vector LinearIns) !(Vector LiveOuts)

instance Pretty LinearCode where
  ppr (LinearCode inss live_outs) =
    vcat [ fillBreak 30 (ppr ins) <+> ppr outs
         | (ins, outs) <- zip (Vec.toList inss) (Vec.toList live_outs) ]

allocRegsGraph :: Int -> FactBase LiveVars -> Graph BcIns O C -> FinalCode
allocRegsGraph arity !livenessInfo !graph =
  let (live_ins, linearInstructions) = lineariseGraph livenessInfo graph in
  -- trace ("GRAPH:\n" ++ pretty graph ++ "\n\n" ++
  --        "LINEAR:\n" ++ pretty (vcat [ ppr ins $$ indent 4 (colour1 (ppr lives))
  --                                    | (ins, lives) <- linearInstructions])
  --         ++ "\n\n") $
  let !allocated = allocRegsLinearScan graph live_ins linearInstructions in
  finaliseCode arity allocated

type LiveOuts = LiveVars
type LiveIns = LiveVars

finaliseCode :: Int -> [LinearIns] -> FinalCode
finaliseCode arity final = -- trace ("FINAL:\n"++pretty (vcat (map ppr final))) $
  let !all_regs = S.fromList (concatMap universeBi final :: [BcVar])
      isReg (BcReg _ _) = True
      isReg _ = False
  in if not (all isReg (S.toList all_regs)) then
       error $ "Register allocator did not assign a register for a variable: " ++
         pretty (vcat (map ppr final))
      else
        let !framesize = maximum [ r + 1 | BcReg r _ <- S.toList all_regs ]
            !code = fixJumpTargets (Vec.fromList final)
        in FinalCode (arity `max` framesize) code

fixJumpTargets :: Vector LinearIns -> Vector (LinearIns' Int)
fixJumpTargets code0 =
   -- trace ("LABELS: " ++ show labelToCode0Index) $
   code1
 where
   -- maps label to offset into code0
   labelToCode0Index :: M.Map Label Int
   labelToCode0Index = Vec.ifoldl' add_label M.empty code0
     where add_label lbls idx (Fst (Label l)) = M.insert l idx lbls
           add_label lbls _   _               = lbls

   !code0_len = Vec.length code0

   -- decides whether to keep the instruction in the output
   keep _idx (Fst _) = False
   keep _idx (Mid (Assign dst (Move src))) = dst /= src
   keep _idx (Mid _) = True
   keep idx (Lst ins)
     | Goto dest_label <- ins
     , idx + 1 < code0_len
     , Fst (Label next_label) <- code0 Vec.! (idx + 1)
     = dest_label /= next_label  -- only keep if jump is not to next instruction
     | otherwise
     = True
   
   keep_code :: Vector Bool
   keep_code = Vec.imap keep code0

   new_idx :: Vector Int
   new_idx = Vec.prescanl' inc_if_keep 0 keep_code
    where inc_if_keep idx keep = if keep then idx + 1 else idx

   labelToCode1Index = M.map (\old_idx -> new_idx Vec.! old_idx) labelToCode0Index

   lookupLabelCode1 l = 
     case M.lookup l labelToCode1Index of
       Just i -> i
       Nothing -> error $ "fixJumpTargets: label missing: " ++ pretty l
   
   updateLabel (Fst ins) = Fst (mapLabels lookupLabelCode1 ins)
   updateLabel (Mid ins) = Mid (mapLabels lookupLabelCode1 ins)
   updateLabel (Lst ins) = Lst (mapLabels lookupLabelCode1 ins)

   code1 = Vec.map updateLabel (Vec.ifilter (\idx _ -> keep_code Vec.! idx) code0)
                            

lineariseGraph :: FactBase LiveVars -> Graph BcIns O C
               -> (LiveIns, [(LinearIns, LiveOuts)])
lineariseGraph livenessInfo graph@(GMany (JustO entry) body NothingO) =
  (entry_live_ins, zip (Vec.toList linear_code') (Vec.toList live_outs))
 where
   body_blocks = postorder_dfs graph  -- excludes entry sequence
   !linear_code =
        Vec.fromList $ 
          concat (lineariseBlock livenessInfo entry :
                  map (lineariseBlock livenessInfo) body_blocks)
   !live_outs = liveOuts livenessInfo linear_code
   !linear_code' = annotateAllocationsWithLiveIns live_outs linear_code

   entry_live_ins
     | Vec.length linear_code == 0 = S.empty
     | otherwise = 
        nonVoid (liveIns livenessInfo (Vec.head linear_code)
                         (Vec.head live_outs))

liveIns :: FactBase LiveVars -> LinearIns -> LiveOuts -> LiveIns
liveIns global_live_outs ins0 live_out =
  case ins0 of
    Lst ins -> live ins global_live_outs
    Mid ins -> live ins live_out
    Fst ins -> live ins live_out

annotateAllocationsWithLiveIns :: Vector LiveOuts -> Vector LinearIns
                               -> Vector LinearIns
annotateAllocationsWithLiveIns lives inss = Vec.imap annotate inss
 where
   annotate :: Int -> LinearIns -> LinearIns
   annotate n (Mid ins@(Assign d (Alloc t args _))) =
     Mid (Assign d (Alloc t args (nonVoid (live ins (lives Vec.! n)))))
   annotate n (Mid ins@(Assign d (AllocAp args _))) =
     Mid (Assign d (AllocAp args (nonVoid (live ins (lives Vec.! n)))))
   annotate n i = i

lineariseBlock :: FactBase LiveVars -> Block BcIns e x -> [LinearIns]
lineariseBlock live_facts block =
  entry_inss ++ map (Mid . force) middles ++ tail_inss
 where
   (entry, middles, tail) = blockToNodeList block
   entry_inss | JustC ins <- entry = [Fst ins]
              | otherwise          = []
   tail_inss
     | JustC (Case typ x targets) <- tail
     = let !targets' = [ (tag, livesAt label, label)
                       | (tag, _, label) <- targets ] in
       [Lst (Case typ x targets')]
     | JustC (Eval succ_label _ reg) <- tail
     = let !lives' = livesAt succ_label in
       [Lst (Eval succ_label lives' reg)]
     | JustC (Call (Just (var, succ_label, _)) fun args) <- tail
     = let !lives' = livesAt succ_label in
       [Lst $! Call (Just (var, succ_label, lives')) fun args]
     | JustC exit_ins <- tail
     = [Lst exit_ins]
     | NothingC <- tail
     = []

   livesAt label = nonVoid $ fromMaybe S.empty (lookupFact label live_facts)

nonVoid :: LiveVars -> LiveVars
nonVoid lives = S.filter (not . isVoid) lives

allocRegsLinearScan :: Graph BcIns O C -> LiveVars -> [(LinearIns, LiveOuts)] -> [LinearIns]
allocRegsLinearScan _ _ [] = []
allocRegsLinearScan graph liveIns linearInstructions@((entry, outs):_) =
  runAllocM liveIns graph (go linearInstructions)
 where
   go ((!ins, !liveOuts):inss) = do
     allocRegsSingle ins liveOuts
     go inss
   go [] = return ()

mkInitialAllocation :: LiveVars -> (M.Map BcVar FinalReg, S.Set FinalReg)
mkInitialAllocation outs =
  let !alloc = M.fromList [ (var, R r) | var@(BcReg r _) <- S.toList outs ] in
  let !all_regs = S.fromList (map R [0..255]) in
  let !regs_without_initials = foldr S.delete all_regs (M.elems alloc) in
  ({- trace ("INITIAL_ALLOC: "++pretty alloc) -} alloc, regs_without_initials)

newtype FinalReg = R Int
  deriving (Eq, Ord)

instance Pretty FinalReg where ppr (R n) = char 'R' <> int n

data AllocState = AS { current :: !RegMapState
                     , pending :: !(M.Map Label RegMapState)   
                     , emitted :: ![LinearIns]
                     , graph :: Graph BcIns O C  -- for error messages only
                     }

-- | The current register assignment.
--
data RegMapState = RMS 
  { var2reg :: !(M.Map BcVar FinalReg)
  , reg2var :: !(M.Map FinalReg BcVar)
  , frees :: !(S.Set FinalReg)
  }

instance Pretty RegMapState where
  ppr rms = vcat [ ppr reg <+> text "<->" <+> ppr var
                 | (var, reg) <- M.toList (var2reg rms) ]

prop_RegMapState_invariant rms =
  var2reg rms == M.fromList [ (var, reg) | (reg, var) <- M.toList (reg2var rms) ] &&
  (M.keysSet (reg2var rms) `S.union` frees rms) == S.fromList [ R r | r <- [0..255] ]

newtype AllocM a = AllocM (State AllocState a)
  deriving (Functor, Monad, MonadState AllocState, Applicative)

runAllocM :: LiveIns -> Graph BcIns O C  -> AllocM a -> [LinearIns]
runAllocM live_ins gr (AllocM m) =
  let (!initial_alloc, !frees0) = mkInitialAllocation live_ins
      !as0 = AS{ current =
                   RMS{ var2reg = initial_alloc
                      , reg2var = M.fromList [ (reg, var)
                                             | (var, reg) <- M.toList initial_alloc ]
                      , frees = frees0
                      }
               , pending = M.empty
               , emitted = []
               , graph = gr }
      !(_res, as') = runState m as0
  in reverse (emitted as')

emit :: LinearIns -> AllocM ()
emit !ins =
  -- trace (">>EMIT: " ++ pretty ins) $ do
  modify' (\as -> as{ emitted = ins : emitted as })

getFreeReg :: AllocM FinalReg
getFreeReg = S.findMin . frees <$> gets current

currentAlloc :: BcVar -> AllocM (Maybe FinalReg)
currentAlloc var = M.lookup var . var2reg <$> gets current

setAlloc :: BcVar -> FinalReg -> AllocM ()
setAlloc var reg =
  modify' (\as -> 
             let curr = current as      
                 curr' =
                   curr{ var2reg = M.insert var reg (var2reg curr)
                       , reg2var = M.insert reg var (reg2var curr)
                       , frees = S.delete reg (frees curr) }
             in as{ current = curr' })

markAsFree :: FinalReg -> AllocM ()
markAsFree reg = do
  mb_owner <- regOwner reg
  case mb_owner of
    Nothing -> return ()  -- reg must be free
    Just owner -> do
      modify' (\as -> 
        let curr = current as
            curr' =
              curr{ var2reg = M.delete owner (var2reg curr)
                  , reg2var = M.delete reg (reg2var curr)
                  , frees = S.insert reg (frees curr) }
        in as{ current = curr' })

setLiveOuts :: LiveOuts -> AllocM ()
setLiveOuts outs = do
  curr <- gets current
  let mapped = M.keysSet (var2reg curr)
  if S.size outs == S.size mapped then
    return ()
   else do
    let todo_remove = mapped `S.difference` outs
    forM_ (S.toList todo_remove) $ \var -> do
      Just reg <- currentAlloc var
      markAsFree reg

rememberState :: Label -> LiveOuts -> AllocM ()
rememberState lbl outs = do
  saved <- gets current  -- remember current state before this point
  setLiveOuts outs
  pendings <- gets pending
  case M.lookup lbl pendings of
    Nothing ->
      modify' (\as -> as{ pending = M.insert lbl (current as) (pending as) })
    Just mapping -> do
      curr <- gets current
      if var2reg curr /= var2reg mapping then
        error "Not-yet-implemented: fix register assignment of inner merges"
       else
        return ()
  modify' (\as -> as{ current = saved })

recallState :: Label -> AllocM ()
recallState lbl = do
  pendings <- gets pending
  case M.lookup lbl pendings of 
    Nothing ->
      error $ "Missing state for label: " ++ show lbl
    Just st ->
      -- trace ("State at " ++ show lbl ++ "\n" ++ pretty st) $ do
      modify' (\as -> as{ current = st })

regOwner :: FinalReg -> AllocM (Maybe BcVar)
regOwner reg = M.lookup reg . reg2var <$> gets current

allocRef1 :: BcVar -> AllocM BcVar
allocRef1 var | isVoid var =
  return (BcReg 0 (varType var))
allocRef1 var = do
  mb_reg <- currentAlloc var
  case mb_reg of
    Just (R r) -> return (BcReg r (varType var))
    Nothing -> do
      gr <- gets graph
      inss <- reverse <$> gets emitted
      rms <- gets current
      error $ "allocRef1 found an unallocated variable: " ++ pretty var
            -- ++ "\nin graph:\n" ++ pretty gr
            ++ "\n\nallocated:\n" ++ pretty (vcat (map ppr inss))
            ++ "\ninvariant = " ++ show (prop_RegMapState_invariant rms)
            ++ "\n\nstate =\n" ++ pretty rms

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

allocDest :: BcVar -> AllocM BcVar
allocDest var@(BcVar _ _) = do
  reg@(R r) <- getFreeReg
  setAlloc var reg
  return (BcReg r (varType var))
allocDest var@(BcReg r t) = do
  mb_owner <- regOwner (R r)
  whenJust mb_owner $ \owner -> do
    rms <- gets current
    error ("Precoloured register not available: R" ++ show r
           ++ "\nstate:\n" ++ pretty rms)
  setAlloc var (R r)
  return var

-- | Ensure that a suitable register is available for the variable.
-- The only interesting case is where the destination variable is a
-- register.
--
-- Assume we want to allocate registers for the instruction
--
-- > r1 <- %X - %Y    ; live-outs: { r1, %X, %Z }
--
-- The target register must be @r1@, thus if any of @%X@ or @%Z@ are
-- already allocated to register @r1@ we must change the assignment
-- for those variables.  We do this by inserting a move instruction
-- before the actual instruction:
--
-- >   ; alloc(%X) = r1, alloc(%Y) = r0, alloc(%Z) = r2
-- > r3 <- r1
-- >   ; now alloc(%X) = r3
-- > r1 <- r3 - r0
--
-- We do not need to emit this move if the variable that is currently
-- assigned to @r1@ is not live-out.  In that case the current
-- instruction will be the last use of @r1@.  Example:
--
-- >   ; alloc(%Y) = r1, alloc(%X) = r0
-- > r1 <- r0 - r1
--
-- Note that this means that the register assignment of a
-- variable may change throughout its lifetime.
--
-- TODO: This /could/ cause issues if we have merging control flow.
-- Currently, such moves are only ever generated right before a
-- tail-call which cannot lead to merging control-flow inside the
-- allocation unit.
--
ensurePrecolouredDestAvail :: BcVar -> LiveOuts -> AllocM ()
ensurePrecolouredDestAvail (BcVar _ _) _ = return ()
ensurePrecolouredDestAvail (BcReg r t) liveOuts = do
  opt_owner <- regOwner (R r)
  case opt_owner of
    Just owner | owner `S.member` liveOuts -> do
      --trace ("Reg " ++ show r ++ " already in use") $ do
      -- Free up register by moving the current content into a different
      -- register (our notion of "spilling")
      R new <- getFreeReg
      let old = r
      let ty = varType owner
      let ins = (Mid (Assign (BcReg new ty) (Move (BcReg old ty))))
      --trace ("EMITTING MOVE: " ++ pretty ins) $ do
      emit ins
      markAsFree (R old)
      setAlloc owner (R new)
    _ ->
      return ()

varType :: BcVar -> OpTy
varType (BcReg _ t) = t
varType (BcVar _ t) = transType t

--detectDeaths :: LiveOuts -> 

allocRef1' :: LiveOuts -> BcVar -> AllocM BcVar
allocRef1' liveOuts var = do
  var'@(BcReg r _) <- allocRef1 var
  -- trace ("ALLOC_REF: " ++ pretty var ++ " => " ++ pretty var') $ do
  when (not (S.member var liveOuts)) $
    -- trace ("FREEING: " ++ pretty r ++ " OUTS=" ++ pretty liveOuts) $ do
    markAsFree (R r)
  return var'

allocRefs :: LiveOuts -> [BcVar] -> AllocM [BcVar]
allocRefs liveOuts vars = do
  vars' <- mapM allocRef1 vars
  pruneDeads liveOuts vars
  return vars'

pruneDeads :: LiveOuts -> [BcVar] -> AllocM ()
pruneDeads liveOuts vars = do
  forM_ (S.toList (S.fromList vars)) $ \var -> do
    when (not (S.member var liveOuts) && not (isVoid var)) $ do
      Just r <- currentAlloc var
      markAsFree r

allocDest' :: LiveOuts -> BcVar -> AllocM BcVar
allocDest' liveOuts var = do
  var'@(BcReg r _) <- allocDest var
  -- trace ("ALLOC_DEST: " ++ pretty var ++ " => " ++ pretty var') $ do
  when (not (S.member var liveOuts)) $
    -- trace ("FREEING: " ++ pretty r ++ " OUTS=" ++ pretty liveOuts) $ do
    markAsFree (R r)
  return var'

allocRegsSingle :: LinearIns -> LiveOuts -> AllocM ()
-- allocRegsSingle ins liveOuts
  -- | trace ("INS: " ++ pretty ins ++ "\n  " ++
  --          pretty (colour1 (ppr liveOuts))) False = return ()
allocRegsSingle (Fst (Label l)) liveOuts = do
  recallState l
  setLiveOuts liveOuts
  emit (Fst (Label l))
allocRegsSingle (Mid ins) liveOuts = do
  case ins of
    -- Assign dest (Alloc c xs lives) -> do
    --   ensurePrecolouredDestAvail dest liveOuts
    --   c':xs' <- allocRefs liveOuts (c:xs)
    --   dest' <- allocDest' liveOuts dest
    --   lives' <- S.fromList <$> mapM allocRef1 (S.toList lives)
    --   emit (Mid (Assign dest' (Alloc c' xs' lives')))
    -- Assign dest (AllocAp xs lives) -> do
    --   ensurePrecolouredDestAvail dest liveOuts
    --   xs' <- allocRefs liveOuts xs
    --   dest' <- allocDest' liveOuts dest
    --   lives' <- S.fromList <$> mapM allocRef1 (S.toList lives)
    --   emit (Mid (Assign dest' (AllocAp xs' lives')))     
    Assign dest rhs -> do
      ensurePrecolouredDestAvail dest liveOuts
      rhs' <- descendBiM allocRef1 rhs
      pruneDeads liveOuts (universeBi rhs)
      dest' <- allocDest' liveOuts dest
      emit (Mid (Assign dest' rhs'))
    Store s1 n s2 -> do
      ins' <- Store <$> allocRef1 s1 <*> pure n <*> allocRef1 s2
      pruneDeads liveOuts [s1, s2]
      emit (Mid ins')
                               
allocRegsSingle (Lst ins) liveOuts = do
  case ins of
    Goto l -> do
      rememberState l liveOuts
      emit (Lst ins)
    Ret1 x -> do
      emit =<< (Lst . Ret1 <$> allocRef1' liveOuts x)
    RetN xs -> do
      xs' <- allocRefs liveOuts xs
      emit (Lst (RetN xs'))
    Call Nothing f xs -> do
      ins <- descendBiM (allocRef1' liveOuts) ins
      emit (Lst ins)
    Call (Just (res, lbl, lives)) f xs -> do
      rms <- gets current
      -- trace ("STATE1:\n"++pretty rms) $ do
      ensurePrecolouredDestAvail res liveOuts
      f':xs' <- allocRefs liveOuts (f:xs)
      res' <- allocDest' liveOuts res
      -- trace ("STATE2:" ++ pretty lives ++ " / " ++ pretty liveOuts ) $ do
      lives' <- S.fromList <$> mapM allocRef1 (S.toList lives)
      -- trace ("STATE3") $ do
      emit (Lst (Call (Just (res', lbl, lives')) f' xs'))
      rememberState lbl lives
    CondBranch op ty x y lbl1 lbl2 -> do
      x' <- allocRef1 x
      y' <- allocRef1 y
      pruneDeads liveOuts [x, y]
      rememberState lbl1 liveOuts
      rememberState lbl2 liveOuts
      emit (Lst (CondBranch op ty x' y' lbl1 lbl2))
    Eval lbl lives x -> do
      x' <- allocRef1' liveOuts x
      rememberState lbl lives
      lives' <- S.fromList <$> mapM allocRef1 (S.toList lives)
      emit (Lst (Eval lbl lives' x'))
    Case ctype x targets -> do
      -- trace ("CASE: " ++ pretty ins) $ do
      let realLiveOuts = liveOuts -- S.unions [ lives | (_, lives, _) <- targets ]
      x' <- allocRef1' realLiveOuts x
      targets' <- forM targets $ \(tag, lives, lbl) -> do
                    lives' <- S.fromList <$> mapM allocRef1 (S.toList lives)
                    rememberState lbl lives
                    return (tag, lives', lbl)
      emit (Lst (Case ctype x' targets'))
    Update -> do
      emit (Lst Update)
    Stop -> do
      emit (Lst Stop)
    _ -> error $ "NYI: " ++ pretty ins

liveOuts :: FactBase LiveVars -> Vector LinearIns -> Vector LiveVars
liveOuts liveInsAtLabels inss =
  -- Vec.map nonVoid $
     Vec.modify loop (Vec.replicate (Vec.length inss) S.empty)
 where
   loop :: MVec.MVector s LiveOuts -> ST s ()
   loop dest = go dest (Vec.length inss - 1)

   liveInsFromLiveOuts :: LinearIns -> LiveOuts -> LiveIns
   liveInsFromLiveOuts (Fst _) liveOuts = liveOuts
   liveInsFromLiveOuts (Mid ins) liveOuts = live ins liveOuts
   liveInsFromLiveOuts (Lst ins) _ = live ins liveInsAtLabels

   liveInsAt :: MVec.MVector s LiveOuts -> Int -> ST s LiveIns
   liveInsAt liveouts i
     | i >= Vec.length inss = return S.empty
     | otherwise 
     = do outs <- MVec.read liveouts i
          let !ins = liveInsFromLiveOuts (inss Vec.! i) outs
          -- trace ("\nOUTS=" ++ pretty outs ++ "\n  => " ++ pretty (inss Vec.! i) ++ "\n  => INS=" ++ pretty ins ++ "\n") $ do
          return ins
   
   go :: MVec.MVector s LiveOuts -> Int -> ST s ()
   -- go _ i | trace ("go " ++ show i) False = undefined
   go liveOuts i | i < 0 = return ()
   go liveOuts i = do
     case inss Vec.! i of
       Fst ins -> do
         live_ins_next <- liveInsAt liveOuts (i + 1)
         MVec.write liveOuts i (nonVoid live_ins_next)
       Mid ins -> do
         live_ins_next <- liveInsAt liveOuts (i + 1)
         MVec.write liveOuts i (nonVoid live_ins_next)
       Lst ins -> do
         let ls = successors ins
         let live_outs =
               S.unions [ fromMaybe S.empty (lookupFact l liveInsAtLabels)
                        | l <- ls ]
         MVec.write liveOuts i (nonVoid live_outs)
     go liveOuts (i - 1)

{-
liveOuts :: FactBase LiveVars -> Vector LinearIns -> Vector LiveVars
liveOuts global_live_outs inss =
  Vec.prescanr' calcLives S.empty inss
 where
   calcLives (Mid ins) live_out = live ins live_out
   calcLives (Fst ins) live_out = live ins live_out
   calcLives (Lst ins) live_out = 
     S.unions [ 
     let ls = labels ins in
    live ins global_live_outs
-}
