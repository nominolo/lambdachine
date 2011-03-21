{-# LANGUAGE PatternGuards, BangPatterns #-}
{-# LANGUAGE GADTs #-}
module Lambdachine.Interp.Trace
  ( IRIns_(..), TRef, mkRecordState, record1, finaliseTrace,
    test_record1, unrollLoop )
where

import Lambdachine.Grin.Bytecode hiding ( (<*>) )
import Lambdachine.Utils
import Lambdachine.Id
import Lambdachine.Interp.Types
import Lambdachine.Builtin

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Applicative hiding ( empty )
import Control.Monad ( forM_, when, mplus )
import Data.Array ( assocs )
import Data.Array.IO
import Data.Array.Unboxed ( UArray, listArray, (!) )
import Data.Generics.Uniplate.Direct ( universeBi, transformBi )
import Data.IORef
--import Data.List
import Data.Maybe
import Data.Monoid
import Data.Graph
import Data.Tree ( Tree(..) )

import qualified Debug.Trace as Tr
--import Debug.Trace

-- -------------------------------------------------------------------

-- | Obtain a reference to the current value in the slot.
--
-- Emit a slot load instruction if slot is currently undefined (i.e.,
-- it is defined outside the trace).
getSlot :: IORef RecordState -> Int -> IO TRef
getSlot rs_ref slot = do
  rs0 <- readIORef rs_ref
  let real_slot = slot + rs_baseslot rs0
  case IM.lookup real_slot (rs_slots rs0) of
    Just ref -> return ref
    Nothing -> do
      ref <- emitRaw rs_ref (SLoad real_slot) -- XXX: or just 'slot'?
      modifyIORef rs_ref $ \rs ->
        rs{ rs_slots = IM.insert real_slot ref (rs_slots rs) }
      return ref

-- | Update the contents of the given stack slot.
setSlot :: IORef RecordState -> Int -> TRef -> IO ()
setSlot rs_ref slot ref = do
  modifyIORef rs_ref $ \rs ->
    let real_slot = slot + rs_baseslot rs in
    rs{ rs_slots = IM.insert real_slot ref (rs_slots rs)
      , rs_modified_slots =
           IM.insert real_slot ref (rs_modified_slots rs) }

-- | Declare the given stack slot as undefined.
--
-- If a slot value is no longer needed (because it is redefined before
-- the next use) we mark the slot as undefined.  This avoids
-- accidentally putting the slot value into a snapshot.
undefineSlot :: IORef RecordState -> Int -> IO ()
undefineSlot rs_ref slot = do
  modifyIORef rs_ref $ \rs ->
    let real_slot = slot + rs_baseslot rs in
    rs{ rs_slots = IM.delete real_slot (rs_slots rs)
      , rs_modified_slots = IM.delete real_slot (rs_modified_slots rs) }

-- | Append the IR instruction to the trace.  Only guards are treated
-- specially; they are annotated with the current snapshot.
emitRaw :: IORef RecordState -> IRIns -> IO TRef
emitRaw rs_ref ir_ins0 = do
  ir_ins <- case ir_ins0 of
              Guard cmp x y snap_ -> do
                snap <- mkSnapshot rs_ref (snap_esc snap_) (snap_loc snap_)
                update_escapees rs_ref snap
                return (Guard cmp x y snap)
              _ -> return ir_ins0
  rs <- readIORef rs_ref
  let ins_id = rs_next rs
  --pprint $ text "*** emitRaw:" <+> ppr ins_id <+> ppr ir_ins
  writeArray (rs_trace rs) ins_id ir_ins
  let tref = TVar ins_id
  writeIORef rs_ref $! rs{ rs_next = 1 + ins_id }
  case ir_ins of
    Loop -> modifyIORef rs_ref $ \rs' -> rs'{ rs_loop_ins = ins_id }
    Phi l r ->
      modifyIORef rs_ref $ \rs' ->
        let (ltr, rtl) = rs_phis rs' in
        rs'{ rs_phis = (IM.insert (refToInt l) r ltr, IM.insert (refToInt r) l rtl) }
    _ -> return ()
  modifyIORef rs_ref $ \rs' ->
    rs'{ rs_ins_to_ref = IM.insert ins_id tref (rs_ins_to_ref rs') }
  return tref

update_escapees :: IORef RecordState -> Snapshot -> IO ()
update_escapees rs snap = do
  rs0 <- readIORef rs
  when (rs_loop_ins rs0 /= 0) $ do
    let escs = [ ref | ref <- IM.elems (snap_slots snap)
                     , isHeapRef ref ]
    writeIORef rs $ 
      rs0{ rs_escapees = foldr S.insert (rs_escapees rs0) escs }

-- | Append the IR instruction to the trace and run the optimisation
-- pipeline on it.
--
-- This is where all forward-optimisations take place.
emit :: IORef RecordState -> InfoTables -> Heap -> IRIns -> IO TRef
emit rs env heap ins = do
  --pprint $ text "... emit:" <+> ppr ins
  --pprint . (text "..." <+>) =<< ppRecordState False =<< readIORef rs
  case ins of
    -- We normally use `setSlot`, but when unrolling a loop we want to
    -- treat `SLoad` instructions properly.
    SLoad n -> do
      ref <- getSlot rs n
      --pprint $ text "...... Slot" <+> ppr n <+> text "=>" <+> ppr ref
      return ref
    Guard cmp@CmpEq x y _
      | x == y -> do
        -- TODO: constant-fold guards
        --pprint $ text "...... Redundant guard:" <+> ppr cmp <+> ppr x <+> ppr y
        return nilTRef
    FLoad x offs
      -- special case for loading the info table.
      | isConstTRef x, TCId addr <- refConst x, offs == 0 -> do
          let Closure itbl_id _ = lookupClosure heap (SLoc addr)
          loadConst rs (TCItbl itbl_id)
      | isHeapRef x ->
          record_fload_heap rs x offs
      -- Otherwise: Regular redundant load removal = CSE, handled below
    AllocN itbl n ->
      record_alloc rs itbl n
    FStore ptr fld val ->
      record_fstore rs ptr fld val
    PushFrame ret_addr node_ref framesize live_outs ->
      record_pushframe rs env heap ret_addr node_ref framesize live_outs
    PopFrame ->
      record_popframe rs
    _ ->
      -- All instructions not handled by the above are handled via
      -- a very simple CSE.
      exactCSE rs ins

record_alloc :: IORef RecordState -> TRef -> Int -> IO TRef
record_alloc rs_ref itbl size = do
  TVar iref <- emitRaw rs_ref (AllocN itbl size)
  let ref = THp iref
  modifyIORef rs_ref $ \rs ->
    rs{ rs_virtual_objects =
          IM.insert (refToInt ref) (size, IM.singleton 0 itbl) 
              (rs_virtual_objects rs) }
  return ref

record_fstore :: IORef RecordState -> TRef -> Int -> TRef -> IO TRef
record_fstore rs_ref ptr field val = do
  modifyIORef rs_ref $ \rs ->
    let !iptr = refToInt ptr in
    case IM.lookup iptr (rs_virtual_objects rs) of
      Nothing ->
        error "record_fstore: Don't know what to do with store to non-fresh object."
      Just (sz, fields) | field >= 0 && field <= sz ->
        rs{ rs_virtual_objects =
               IM.insert iptr (sz, IM.insert field val fields)
                         (rs_virtual_objects rs) }
  emitRaw rs_ref (FStore ptr field val)
  return nilTRef

record_fload_heap :: IORef RecordState -> TRef -> Int -> IO TRef
record_fload_heap rs_ref ptr offs =
 assert (isHeapRef ptr) $ do
  rs <- readIORef rs_ref
  case IM.lookup (refToInt ptr) (rs_virtual_objects rs) of
    Nothing -> error "Reading from non-existend heap object"
    Just (_sz, fields) ->
      return $ fromMaybe nilTRef $ IM.lookup offs fields

directRefs :: IRIns -> [TRef]
directRefs (Guard _ x y _) = [x, y]
directRefs ins = universeBi ins

exactCSE :: IORef RecordState -> IRIns -> IO TRef
exactCSE rs_ref ins = do
  -- backwards search for exact same instruction
  let limit = maximum (1:[ n | TVar n <- directRefs ins ])
  rs <- readIORef rs_ref
  let trace = rs_trace rs
      from = rs_next rs - 1
  --pprint $ text "***CSE:" <> ppr from <> char '-' <> ppr limit <+> ppr ins
  let find_same i
        | i > limit = do
            ins' <- readArray trace i
            if sameIns ins ins' then do
              --pprint $ text "...... CSE:" <+> ppr ins <+> text "=>" <+> ppr i
              return (TVar i)
             else find_same (i - 1)
        | otherwise = do
          emitRaw rs_ref ins
  find_same from
 where
   sameIns :: IRIns -> IRIns -> Bool
   sameIns (Guard cmp x y _) (Guard cmp' x' y' _) =
     --Tr.trace ("***Comparing guards***") $
     cmp == cmp' && x == x' && y == y'
   sameIns i j = i == j

ppSlots :: RecordState -> PDoc
ppSlots rs0 = pp_slots rs0 (rs_slots rs0) <> char '}'
 where
   pp_slots _rs slots | IM.null slots = text "{"
   pp_slots rs slots =
     let slot_keys = IM.keysSet slots
         base = rs_baseslot rs
         top = rs_topslot rs
         slot_low = min (IS.findMin slot_keys) base
         slot_hi  = (IS.findMax slot_keys) `max` base `max` top
         esc@(esc_gr, _) = mkEscapeGraph rs (rs_escapees rs)
         cyc = findCycles esc
     in text "<<base=" <> ppr base <> text ",top=" <> ppr top <> text ">>" $+$
        --text "EscGraph =" <+> ppGraph esc_gr $+$
        --text "EscSCCs  =" <+> ppForest (filter (not . null . subForest) (scc esc_gr)) $+$
        --text "Unsinkable =" <+> ppr cyc $+$
        --text "Escapees =" <+> ppr (rs_escapees rs) $+$
        --text "Sinkable =" <+> ppr (S.filter (isSinkable rs esc cyc) (rs_escapees rs)) $+$
        (collect' (text "Slots: {" <> ppr slot_low <> char ':')
                 [slot_low..slot_hi] $ \doc s ->
          doc <>
          (if s == base || s == top then char '|' else
             if s `mod` 5 == 0 then char '.' else char ' ') <>
          (case IM.lookup s slots of
             Nothing -> ppr s <> char '=' <> text "-----"
             Just t -> ppr s <> char '=' <>
               (if IM.member s (rs_modified_slots rs) then
                 underline (ppr t) else ppr t)))

mkRecordState :: InfoTableId -> Int -> IO (IORef RecordState)
mkRecordState root_id framesize = do
  trace <- newArray_ (1, 2000)
  newIORef $
    RecordState{ rs_trace_root = root_id
               , rs_slots = mempty
               , rs_modified_slots = mempty
               , rs_baseslot = 0
               , rs_topslot = framesize
               , rs_trace = trace
               , rs_next = 1
               , rs_consts = mempty
               , rs_next_const = 1
               , rs_virtual_objects = mempty
               , rs_next_virt_obj = 1
               , rs_stackframes = EntryFrame
               , rs_loop_ins = 0
               , rs_escapees = mempty
               , rs_equi_escape = mempty
               , rs_phis = (mempty, mempty)
               , rs_ins_to_ref = mempty
               }

ppRecordState :: Bool -> RecordState -> IO PDoc
ppRecordState insts rs = do
  pp1 <- if not insts then return empty else do
           inss <- take (rs_next rs - 1) `fmap` getAssocs (rs_trace rs)
           return $ vcat (map (\(i, ins) -> ppFill 4 i <+> ppr ins) inss) <> linebreak
  let pp2 = ppSlots rs
  return $ pp1 <> pp2 $+$ pp_heap (IM.toList $ rs_virtual_objects rs)
--         $+$ text "n => ref =" <+> ppr (rs_ins_to_ref rs)

loadConst :: IORef RecordState -> TConst -> IO TRef
loadConst rs_ref bc_const = do
  rs <- readIORef rs_ref
  case M.lookup bc_const (rs_consts rs) of
    Just tref -> return tref
    Nothing -> do
      let tref = TCst (rs_next_const rs) bc_const
      writeIORef rs_ref $! rs{ rs_next_const = 1 + rs_next_const rs
                             , rs_consts = M.insert bc_const tref (rs_consts rs) }
      --pprint $ text "Const" <+> ppr tref <+> char '=' <+> ppr bc_const
      return tref

mkSnapshot :: IORef RecordState -> S.Set BcVar -> (InfoTable, Int)
           -> IO Snapshot
mkSnapshot rs_ref frame_escapes (itbl, pc) = do
  rs <- readIORef rs_ref
  let base = rs_baseslot rs
  let possible_escapes = IS.fromList $
        [(-1) .. (base - 1)] ++
        [ base + i | BcReg i <- S.toList frame_escapes ]

  let slots = IM.fromDistinctAscList $
                [ (i, ref) | (i, ref) <- IM.toList (rs_modified_slots rs)
                           , i `IS.member` possible_escapes ]
  return $ Snapshot
    { snap_slots = slots
    , snap_base = base
    , snap_top = rs_topslot rs
    , snap_allocs = mempty
    , snap_heap = trans_closure (IM.elems slots) (rs_virtual_objects rs)
    , snap_esc = frame_escapes
    , snap_loc = (itbl, pc)
    }
 where
   trans_closure :: [TRef] -> IM.IntMap (Int, IM.IntMap TRef)
                 -> M.Map TRef (Int, IM.IntMap TRef)
   trans_closure roots heap = go roots S.empty M.empty
    where
      go :: [TRef] -> S.Set TRef -> M.Map TRef (Int, IM.IntMap TRef)
         -> M.Map TRef (Int, IM.IntMap TRef)
      go [] _seen acc = acc
      go (r:rs) seen acc
        | r `S.member` seen || not (isHeapRef r)
         = go rs seen acc
        | otherwise
         = case IM.lookup (refToInt r) heap of
             Nothing -> go rs (S.insert r seen) acc
             Just x@(_,new_roots) ->
               go (IM.elems new_roots ++ rs) (S.insert r seen)
                  (M.insert r x acc)

record1 :: InfoTableId
        -> InfoTables -> Heap -> ArgStack -> InfoTable -> Int
        -> FinalIns -> IORef RecordState
        -> IO Bool -- ^ Keep recording?
record1 root env heap args itbl pc ins rs = do
  --pprint =<< ppRecordState False =<< readIORef rs
  case ins of
    Mid (Assign (BcReg r) rhs) -> record_rhs r rhs
    Lst (Eval _pc' liveouts (BcReg reg)) ->
      record_eval env heap args itbl pc reg rs
                  [ i | BcReg i <- S.toList liveouts ]
    Lst (Ret1 (BcReg reg)) -> record_return reg
    Lst (Call Nothing f params) -> record_tailcall f params
    Lst (Case CaseOnTag reg alts) -> record_case reg alts
    Lst Update -> record_update
 where
   record_rhs dst (Move (BcReg src)) = do
     ref <- getSlot rs src
     setSlot rs dst ref
     return True
   record_rhs dst (BinOp op _ty (BcReg src1) (BcReg src2)) = do
     tref1 <- getSlot rs src1
     tref2 <- getSlot rs src2
     tref3 <- emit rs env heap (Op op tref1 tref2)
     setSlot rs dst tref3
     return True
   record_rhs dst (Load (LoadLit l)) = do
     tref <- loadConst rs (bc_const_to_tconst l)
     setSlot rs dst tref
     return True
   record_rhs dst (Load (LoadGlobal x)) = do
     tref <- case idDetails x of
               DataConInfoTableId -> loadConst rs (TCItbl (mkItblId x))
               InfoTableId -> loadConst rs (TCItbl (mkItblId x))
               _ -> loadConst rs (TCId x)
     setSlot rs dst tref
     return True
   record_rhs dst (Load LoadSelf) = do
     node_ptr <- getSlot rs (-1)
     setSlot rs dst node_ptr
     return True
   record_rhs dst (Load (LoadClosureVar offs)) = do
     node_ptr <- getSlot rs (-1)
     tref <- emit rs env heap (FLoad node_ptr offs)
     setSlot rs dst tref
     return True
   record_rhs dst (Fetch (BcReg ptr) offs) = do
     tref <- getSlot rs ptr
     tref' <- emit rs env heap (FLoad tref offs)
     setSlot rs dst tref'
     return True
   record_rhs dst (Alloc (BcReg f) vars) = do
     let nvars = length vars
     fref@(TCst _ (TCItbl _)) <- getSlot rs f
--     when (not (isConstTRef fref)) $ error "Alloc with non-constant itbl"
     ptr <- emit rs env heap (AllocN fref nvars)
     forM_ (zip [1..nvars] vars) $ \(i, (BcReg r)) -> do
       ref <- getSlot rs r
       emit rs env heap (FStore ptr i ref)
     setSlot rs dst ptr
     return True

   record_tailcall (BcReg fn) params = do
     fnptr <- readReg args fn
     let Closure itbl_id _ = lookupClosure heap fnptr
         itbl'@CodeInfoTable{} = lookupInfoTable env itbl_id
     -- TODO: abort if back at root

     fnref <- getSlot rs fn
     itbl_ref <- emit rs env heap (FLoad fnref 0)  -- get info table
     expected_itbl <- loadConst rs (TCItbl itbl_id)

     let snap = ProtoSnapshot (S.fromList $ params) (itbl, pc)
     emit rs env heap (Guard CmpEq itbl_ref expected_itbl snap)
     setSlot rs (-1) fnref
     refs <- mapM (\(BcReg r) -> getSlot rs r) params
     forM_ (zip [0..] refs) $ \(i, ref) -> setSlot rs i ref
     modifyIORef rs $ \rs' ->
       rs'{ rs_topslot = rs_baseslot rs' + fc_framesize (itblCode itbl') }
     mapM (undefineSlot rs) [length params .. fc_framesize (itblCode itbl')]
     if root == itbl_id then do emitRaw rs Loop >> return False
      else return True

   record_return rslt = do
     --error "Unimplemented: RET1"
     -- Return bytecode address is in slot -2, from there we have to
     -- figure out the slot to which we have to write the result.
     --
     -- We only need to put a guard on the return address, because
     -- if the return address points to the expected bytecode, then
     -- the info table for the node pointer in the returned-to frame
     -- must match.  This also determines the base size.
     --
     -- Return base is in slot -3.  The result then has to be written
     -- relative to this new base.  This is the tricky bit.
     --
     let ArgStack base top stack = args
     ValI prev_base <- readArray stack (base - 3)
     ValI prev_pc <- readArray stack (base - 2)
     let old_base = fromIntegral prev_base :: Int
         old_pc = fromIntegral prev_pc :: Int
     old_node <- readArray stack (old_base - 1)
     let Closure itbl_id _ = lookupClosure heap old_node
         itbl' = lookupInfoTable env itbl_id

     expected_ret_addr <- loadConst rs (TCPC itbl_id old_pc)
     actual_ret_addr <- getSlot rs (-2)

     let snap = ProtoSnapshot (S.fromList [ BcReg r | r <- [0..top - base - 1]])
                              (itbl, pc)
     emit rs env heap (Guard CmpEq actual_ret_addr expected_ret_addr snap)

     result_ref <- getSlot rs rslt
     let dst :: Int
         dst =
           case fc_code (itblCode itbl') V.! (old_pc - 1) of
             Lst (Call (Just (BcReg dst', _, _)) _ _) -> dst'
             Lst (Eval _ _ (BcReg dst')) -> dst'

     emit rs env heap PopFrame

     setSlot rs dst result_ref
     return True

   record_update = do
     new_ptr <- getSlot rs 1
     old_ptr <- getSlot rs 0
     -- TODO: load indirection Itbl
     emitRaw rs (UpdateR old_ptr new_ptr)
     record_return 1

   record_case :: BcVar -> [(BcTag, S.Set BcVar, label)] -> IO Bool
   record_case bc@(BcReg reg) alts = do
     -- We don't actually need to look at the alternatives, because
     -- the interpreter will do the selection and then call record
     -- on the appropriate instruction.  We just have to emit the
     -- proper guard here.

     ptr <- readReg args reg
     let Closure itbl_id _ = lookupClosure heap ptr
         ConstrInfoTable{ itblTag = tag } = lookupInfoTable env itbl_id
         esc = escaping_slots tag alts

     -- Emit a guard on the info table pointer
     ref <- getSlot rs reg
     itbl_ref <- emit rs env heap (FLoad ref 0)
     expected_itbl <- loadConst rs (TCItbl itbl_id)
     --snap <- mkSnapshot rs
     let snap = ProtoSnapshot (S.insert bc esc) (itbl, pc)
     emit rs env heap (Guard CmpEq itbl_ref expected_itbl snap)
     return True

   bc_const_to_tconst (CInt n) = TCInt n
   bc_const_to_tconst (CStr s) = TCStr s

   escaping_slots :: Int -> [(BcTag, S.Set BcVar, a)] -> S.Set BcVar
   escaping_slots tag alts_ =
     let (dflt_lives, alts0)
           | (DefaultTag, l, _):alts' <- alts_ = (l, alts')
           | otherwise = (S.empty, alts_)
         -- Collect the live-out variables for all branches that have
         -- *not* been taken.  If we reach the end of the alts, there
         -- may have been a default case.
         collect_exit_lives found_match acc []
           | found_match = acc `S.union` dflt_lives
           | otherwise = acc -- No match, default branch will be taken
         collect_exit_lives found_match acc ((Tag tag', l, _):alts)
           | tag == tag' -- this branch matches, *don't* add live vars
           = collect_exit_lives True acc alts
           | otherwise
           = collect_exit_lives found_match (l `S.union` acc) alts
     in
       collect_exit_lives False S.empty alts0

record_eval :: InfoTables -> Heap -> ArgStack -> InfoTable -> Int
            -> Int -> IORef RecordState -> [Int] -> IO Bool
record_eval env heap args itbl pc reg rs liveouts = do
  -- We specialise on the info table, hence we need to get its actual
  -- runtime value.
  ptr <- readReg args reg
  let Closure itbl_id _ = lookupClosure heap ptr

  -- Now emit the trace code and guard.  Note: field 0 = info table
  tref <- getSlot rs reg
  itblref <- emit rs env heap (FLoad tref 0)
  -- load the current info table as a constant
  expected_itbl <- loadConst rs (TCItbl itbl_id)
  --let ArgStack base top _ = args
  snap <- mkSnapshot rs (S.fromList [ BcReg i | i <- liveouts]) (itbl, pc)
  emit rs env heap (Guard CmpEq itblref expected_itbl snap)

  -- Now, statically figure out what would happen next:
  case lookupInfoTable env itbl_id of
    ConstrInfoTable{} ->
      -- Simple, we don't need to do anything.
      return True
    obj@CodeInfoTable{ itblClosureType = cl_ty }
      | CtFun _ <- cl_ty
      -> return True
      | otherwise
      -> do
        -- It's a thunk or CAF, we will enter its evaluation code.
        -- Put the update frame and stack frame generation code into
        -- the trace.

        -- Push the update frame
        let node_ptr = tref  -- the thing to update
        upd_closure <- loadConst rs (TCId updateItblId)
        (itbl', pc') <- allocStackFrame rs env heap itbl pc upd_closure (mkItblId updateItblId) [node_ptr] [0]
        _ <- allocStackFrame rs env heap itbl' pc' tref (itblId obj) [] liveouts
{-

        let framesize = fc_framesize (itblCode itbl)
        -- 1. Get the actual base pointer (i.e., don't dereference it)
        -- 2. Store it in Mem[base + framesize]
        -- 3. Save current PC (as a constant)
        -- 4. Save update closure (as a constant)
        -- 5. Write
        let ArgStack base _ _ = args
        base_ref <- loadConst rs (TCInt (fromIntegral base))
        setSlot rs framesize base_ref
        pc_ref <- loadConst rs (TCPC itbl_id (pc+1))
        setSlot rs (framesize + 1) pc_ref
        updid_ref <- loadConst rs (TCId updateItblId)
        setSlot rs (framesize + 2) updid_ref
        node_ptr <- getSlot rs (-1)
        setSlot rs (framesize + 3) tref
        modifyIORef rs $ \rs' ->
          rs'{ rs_baseslot = rs_baseslot rs' + framesize + 3 }
-}
        return True


-- | Unroll the loop once.
--
-- Basic idea: The recorded loop will contain lots of guards which are
-- needed because we don't know the context in which we're called.  On
-- the second iteration most of these should be unnecessary.
-- Therefore we unroll the recorded loop once by simply copying the
-- recorded instructions and keep a substitution to make sure
-- instructions use the same operands.  In fact, we don't just copy
-- the instructions, but re-'emit' the instructions through the
-- regular optimisation pipeline, thus redundant instructions are removed
-- automatically.
-- 
-- We separate the two copies of the loop body via the @LOOP@ marker.
-- When control flow reaches the end of the trace execution continues
-- after the @LOOP@ marker, not at the beginning of the trace.  The
-- part prior to the marker therefore becomes the loop header and
-- contains all the loop-invariant pieces of code.
--
-- Code below the @LOOP@ marker now has two control flow predecessors,
-- so we need @PHI@ nodes to keep SSA form.  However, it is easier to
-- emit these at the end of the trace.  This also causes no problems
-- with their operational interpretation as move instructions.
unrollLoop :: IORef RecordState -> InfoTables -> Heap -> IO ()
unrollLoop rs_ref env heap = do
  loop_ref <- TVar . (+(-1)) . rs_next <$> readIORef rs_ref
  go loop_ref 1 M.empty mempty
 where
   -- Stop once we reach LOOP
   go :: TRef -> Int -> M.Map TRef TRef -> S.Set TRef -> IO ()
   go loop_ref !i renaming phis = do
     instrs <- rs_trace <$> readIORef rs_ref
     ins <- readArray instrs i
     case ins of
       Loop -> do
         --pprint $ text "PHIs =" <+> ppr phis 
         --        $+$ text "Subst =" <+> ppr renaming
         insert_phis rs_ref renaming phis
         return ()
       _ -> do
         -- This is part of a hack to figure out whether the
         -- instruction has been constant-folded, or otherwise
         -- eliminated: Get the value of rs_next before and after the
         -- 'emit', if it changed, then the instruction has been
         -- added.
         old_next <- rs_next <$> readIORef rs_ref

         let ins' = rename renaming ins
         case ins' of
           Guard _cmp _x _y snap -> do
             cur_base <- rs_baseslot <$> readIORef rs_ref
             forM_ (IM.toList (snap_slots snap)) $ \(j,r) ->
               setSlot rs_ref (j - cur_base) r  -- @r@ is already renamed
           _otherwise -> return ()
         --pprint $ text "...... Rn:" <+> ppr ins <+> text "~~>" <+> ppr ins'
         ref <- emit rs_ref env heap ins'

         new_next <- rs_next <$> readIORef rs_ref
         let folded = new_next == old_next
         
         -- In some cases the inserted object may be different from
         -- `ins'` (e.g., a different snapshot), so let's re-read the
         -- object.
         ins'' <- if folded then return ins' else
                     readArray instrs old_next
         
         let phis'
               | folded = phis -- The instruction has been folded
               | otherwise =
                 let cross_loop_refs =
                       [ r | r <- universeBi ins'',
                             r < loop_ref, r /= nilTRef, not (isConstTRef r) ]
                 in foldr S.insert phis cross_loop_refs

         let !renaming'
               | ref == nilTRef = renaming
               | otherwise =
                 let oldref = case ins of
                                AllocN _ _ -> THp i
                                _ -> TVar i
                 in M.insert oldref ref renaming
         go loop_ref (i + 1) renaming' phis'

   rename :: M.Map TRef TRef -> IRIns -> IRIns
   rename renaming ins = transformBi renameRef ins
    where
      renameRef r = fromMaybe r (M.lookup r renaming)

   calc_phis :: RecordState -> M.Map TRef TRef -> S.Set TRef 
             -> [IRIns]
   calc_phis rs renaming phis =
     map (\(phi,phi') -> Phi phi phi') $
     S.toList $ S.fromList $
     [ (phi, phi') |
       phi0 <- S.toList phis,
       let Just phi0' = M.lookup phi0 renaming,
       (phi, phi') 
         <- case (,) <$> IM.lookup (refToInt phi0) (rs_virtual_objects rs)
                     <*> IM.lookup (refToInt phi0') (rs_virtual_objects rs) of
              Nothing -> [(phi0, phi0')]
              Just ((_sz1,fields1), (_sz2,fields2)) ->
                --Tr.trace ("MUA " ++ pretty (fields1, fields2)) $
                (phi0, phi0') : zip (IM.elems fields1) (IM.elems fields2),
       phi /= phi', not (isConstTRef phi), not (isConstTRef phi') ]
   
   insert_phis :: IORef RecordState -> M.Map TRef TRef -> S.Set TRef
               -> IO ()
   insert_phis rs renaming phis = do
     rs0 <- readIORef rs
     mapM_ (emitRaw rs) $ calc_phis rs0 renaming phis
{-   forM_ (S.toList phis) $ \phi ->
       case M.lookup phi renaming of
         Just phi' 
           | phi /= phi' -> do
             emitRaw rs (Phi phi phi')
             rs0 <- readIORef rs
             case (,) <$> M.lookup phi (rs_virtual_objects rs0)
                      <*> M.lookup phi' (rs_virtual_objects rs0) of
               Just ((sz1,fields1), (sz2,fields2)) ->
                 
                 return ()
               _ -> 
                 return ()
           | otherwise -> return ()
-}

-- | Sink escaping stores into snapshots wherever possible.
--
--
sinkAllocs :: IORef RecordState -> IO ()
sinkAllocs rs_ref = do
  rs <- readIORef rs_ref
  let esc = mkEscapeGraph rs (rs_escapees rs)
      cyc = findCycles esc
  putStrLn "*** SINKING ALLOCS ***"
  forM_ [rs_loop_ins rs + 1 .. rs_next rs - 1] $ \i -> do
    ins <- readArray (rs_trace rs) i
    case ins of
      Guard cmp opA opB snap -> do
        let !snap' = upd_snapshot esc cyc snap
        writeArray (rs_trace rs) i (Guard cmp opA opB snap')
      _ -> return ()
  -- Update escapees to only include unsinkables.
  writeIORef rs_ref 
    rs{ rs_escapees = S.filter (not . isSinkable esc cyc) (rs_escapees rs) }
 where
   upd_snapshot esc cyc snap@Snapshot{} =
     let slots = IM.elems (snap_slots snap)
         sinkees0 =
           [ ref | ref <- slots, isHeapRef ref, isSinkable esc cyc ref ]
         sink !acc [] = acc
         sink !acc (r:rs)
           | r `S.member` acc = sink acc rs
           | otherwise =
             let Just (_,fields) = M.lookup r (snap_heap snap) in
             sink (S.insert r acc) 
                ([ r' | r' <- IM.elems fields, isHeapRef r',
                        isSinkable esc cyc r' ] ++ rs)
     in snap{ snap_allocs = sink S.empty sinkees0 }

-- | Eliminate dead code.  Replaces dead instructions by 'Nop'.
--
-- An instruction is dead iff there is no use site or the use site is
-- itself dead code.
--
--   - Any sunken allocation is definitely dead.  If it is sunken on
--     one guard it must be sunken on /all/ guards.
--     
--   - If the allocation escapes and cannot be sunken, then the
--     allocation instruction cannot be dead.
-- 
-- Therefore, we mark all allocation nodes reachable from the escapees.
-- Any other allocations are dead code (and so are their stores).
--
elimDeadCode :: IORef RecordState -> IO ()
elimDeadCode rs_ref = do
  rs <- readIORef rs_ref
  let (gr, the_id) = mkEscapeGraph rs (rs_escapees rs)
  let reachable_from_escapes = forestToSet $
        dfs gr [ the_id n | n <- S.toList (rs_escapees rs) ]
  putStrLn "*** Eliminating Dead Code *****"
  --pprint reachable_from_escapes


  mapTraceIRs rs $ \r ir ->
    case ir of
      AllocN _ _
       | not (the_id r `IS.member` reachable_from_escapes) 
       -> Nop
      FStore r' _ _ 
       | not (the_id r' `IS.member` reachable_from_escapes)
       -> Nop
      Phi r1 r2
       | isHeapRef r1 && not (the_id r1 `IS.member` reachable_from_escapes) ||
         isHeapRef r2 && not (the_id r2 `IS.member` reachable_from_escapes)
       -> Nop
      _ -> ir

  -- TODO: Currently only eliminates dead stores.  Do regular DCE here.

  putStrLn "-------------------------------"
  return ()

buildTrace :: IORef RecordState -> IO (InfoTableId, Trace)
buildTrace rs_ref = do
  rs <- readIORef rs_ref
  irs <- take (rs_next rs - 1) <$> getElems (rs_trace rs)
  -- TODO: Remove unnecessary instructions (e.g., Nop).  Need to
  -- rename variables for this, though.

  (_, exits) 
    <- foldTraceIRs rs (1, mempty) $ \ref ir s@(n, exits) -> do
         case ir of
           Guard _ _ _ _ ->
             let !n' = n + 1 in
             return (n', IM.insert (refToInt ref) n exits)
           _ -> return s
  -- TODO: Assign numbers to exits.  Then add (mutable) table to Trace
  -- which maps exit number to trace (if present).
  return (rs_trace_root rs, Trace (V.fromList irs) exits)
 where
   boring Nop = True
   boring (PushFrame _ _ _ _) = True
   boring PopFrame = True
   boring _ = False

-- | Emit IR for allocating a stack frame and adjust record state.
--
allocStackFrame :: IORef RecordState
                -> InfoTables -- To look things up
                -> Heap
                -> InfoTable -- Current info table
                -> Int  -- Current PC
                -> TRef  -- The function we're calling
                -> InfoTableId -- its expected info table (used for
                               -- setting up the stack frame
                -> [TRef] -- Arguments
                -> [Int]  -- Live-outs at return address
                -> IO (InfoTable, Int)
allocStackFrame rs env heap itbl pc node_ref node_id args liveouts = do
  pc_ref <- loadConst rs (TCPC (itblId itbl) (pc + 1))
  let itbl'@CodeInfoTable{} = lookupInfoTable env node_id
      framesize = fc_framesize (itblCode itbl')
  emit rs env heap (PushFrame pc_ref node_ref framesize liveouts)
  forM_ (zip [0..] args) $ \(i, arg) -> setSlot rs i arg
  return (itbl', 0)

record_pushframe :: IORef RecordState -> InfoTables -> Heap
                 -> TRef -> TRef -> Int -> [Int] -> IO TRef
record_pushframe rs _env _heap ret_ref node_ref framesize live_outs = do
  rs0 <- readIORef rs
  let top = rs_topslot rs0
      base = rs_baseslot rs0
      top_slot = top - base
      cur_framesize = top - base
  setSlot rs top_slot (TBase base)
  setSlot rs (top_slot + 1) ret_ref
  setSlot rs (top_slot + 2) node_ref
  invalidate_non_liveouts cur_framesize
  modifyIORef rs $ \rs' ->
    let baseslot' = top + 3 in
    rs'{ rs_baseslot = baseslot'
       , rs_topslot = baseslot' + framesize }
  _ <- emitRaw rs (PushFrame ret_ref node_ref framesize live_outs)
  return nilTRef
 where
   invalidate_non_liveouts size = go 0 live_outs
    where
      --  go _ [] = return ()
          go n _  | n >= size = return ()
          go n ls0@(l:ls)
            | n /= l = undefineSlot rs n >> go (n + 1) ls0
            | n == l = go (n + 1) ls
          go n [] = undefineSlot rs n >> go (n + 1) []

record_popframe :: IORef RecordState -> IO TRef
record_popframe rs = do
  -- FIXME: Make sure there's at least one frame left to pop off.
  rs0 <- readIORef rs
  ret_base <- getSlot rs (-3)
  case ret_base of
    TBase b' -> do
      ret_addr <- getSlot rs (-2)
      let framesize = rs_topslot rs0 - rs_baseslot rs0
      forM_ [(-3) .. framesize - 1] $ undefineSlot rs
      modifyIORef rs $ \rs' ->
        rs'{ rs_baseslot = b'
           , rs_topslot = rs_baseslot rs' - 3 }
      _ <- emitRaw rs PopFrame
      return ret_addr
    _ ->
      fail "record_popframe: Ill-formed frame"

{-
allocStackFrame rs itbls itbl pc node_ref node_id args = do
  let top = fc_framesize (itblCode itbl)
  base_depth <- rs_baseslot `fmap` readIORef rs
  base_ref <- emitRaw rs (LoadBase base_depth)
  setSlot rs top base_ref
  pc_ref <- loadConst rs (TCPC (itblId itbl) (pc + 1))
  setSlot rs (top + 1) pc_ref
--  node_ref <- loadConst rs (TC node)
  setSlot rs (top + 2) node_ref
  let itbl'@CodeInfoTable{} = lookupInfoTable itbls node_id
  --    framesize = fc_framesize code
  forM_ (zip [top + 3..] args) $ \(i, arg) -> setSlot rs i arg
  modifyIORef rs $ \rs' ->
    let baseslot' = rs_baseslot rs' + top + 3 in
    rs'{ rs_baseslot = baseslot'
       , rs_topslot = baseslot' + fc_framesize (itblCode itbl') }
  return (itbl', 0)
-}


finaliseTrace :: IORef RecordState -> InfoTables -> Heap
              -> IO (InfoTableId, Trace)
finaliseTrace rs_ref env heap = do
  --pprint =<< ppRecordState True =<< readIORef rs_ref
  pprint $ text "Unrolling loop"
  unrollLoop rs_ref env heap
  sinkAllocs rs_ref
  elimDeadCode rs_ref
  buildTrace rs_ref
--  pprint $ text "Unrolling loop DONE"
--  pprint =<< ppRecordState True =<< readIORef rs_ref

isLoopInvariant :: RecordState -> TRef -> Bool
isLoopInvariant rs ref = 
  isConstTRef ref ||
  ref == nilTRef ||
  (refToInt ref < rs_loop_ins rs &&
  (case IM.lookup (refToInt ref) $ fst $ rs_phis rs of
     Nothing -> True
     Just ref' -> isLoopInvariant rs ref'))
  
phiTwin :: RecordState -> TRef -> Maybe TRef
phiTwin rs ref =
  IM.lookup refi ltr `mplus` IM.lookup refi rtl
 where
   refi = refToInt ref
   (ltr, rtl) = rs_phis rs

-- | An allocation is sinkable /iff/ it is /only/ needed when the exit
-- is taken.
--
-- This means we can remove the allocation from the trace (and
-- therefore the common case) and only need to construct the heap
-- object when the side exit is taken.  The disadvantage is that it
-- likely increases register pressure.  Instead of keeping alive a
-- single pointer to a heap object, all values store in the heap
-- object need to be kept alive.
--
-- An allocation is unsinkable if it is involved in a loop, i.e., the
-- object contains a direct or indirect reference to itself (which
-- involves @PHI@ nodes).
--
isSinkable :: EscapeGraph -> IS.IntSet -> TRef -> Bool
isSinkable _esc _cycles ref | not (isHeapRef ref) = True
isSinkable (_,the_id) cycles ref = not (the_id ref `IS.member` cycles)
{-  
  the_id 
  case IM.lookup (refToInt ref) (rs_virtual_objects rs) of
    Just (_sz, fields) ->
      and [ field_ok f {- && maybe True field_ok (phiTwin rs f) -}
              | f <- IM.elems fields ]
 where
   field_ok ref' = 
     not (isHeapRef ref') || isLoopInvariant rs ref' || isSinkable rs esc ref'
-}
-- | Computes the set of nodes that are contained in cycles.  This
-- includes nodes that link back to itself.
findCycles :: EscapeGraph -> IS.IntSet
findCycles (esc_gr, _the_id) = forestToSet sccs
  where
    sccs = filter is_real_loop (scc esc_gr)
    children n = esc_gr ! n
    is_real_loop node@Node{rootLabel = n} =
      not (null (subForest node)) || n `elem` children n

forestToSet :: [Tree Vertex] -> IS.IntSet
forestToSet f = forest_to_set IS.empty f
 where
    forest_to_set !acc [] = acc
    forest_to_set !acc (t:ts) =
      forest_to_set (IS.insert (rootLabel t) acc) (subForest t ++ ts)

mapTraceIRs :: RecordState -> (TRef -> IRIns -> IRIns) -> IO ()
mapTraceIRs rs f = do
  forM_ [1 .. rs_next rs - 1] $ \i -> do
    ins <- readArray (rs_trace rs) i
    let Just r = IM.lookup i (rs_ins_to_ref rs)
    let !ins' = f r ins
    writeArray (rs_trace rs) i ins'

foldTraceIRs :: RecordState -> a -> (TRef -> IRIns -> a -> IO a) -> IO a
foldTraceIRs rs a0 f = go 1 a0
 where
   iend = rs_next rs - 1
   go !i a | i > iend = return a
   go !i a = do
    ins <- readArray (rs_trace rs) i
    let Just r = IM.lookup i (rs_ins_to_ref rs)
    a' <- f r ins a
    go (i + 1) a'
  

ppGraph :: Graph -> PDoc
ppGraph g =
  align $ fillSep $ punctuate (char ';') $ concatMap pp (assocs g)
 where
   pp (i, []) = []
   pp (i, cs) = [ppr i <> char ':' <> hcat (punctuate (char ',') (map ppr cs))]

ppForest :: Pretty a => Forest a -> PDoc
ppForest ts = align $ vcat (map ppTree ts)

ppTree :: Pretty a => Tree a -> PDoc
ppTree (Node l cs) =
  align $ ppr l <> char ':' <+> indent 2 (ppForest cs)

type EscapeGraph = (Graph, TRef -> Vertex)

-- | Build a graph of all nodes reachable from the given roots.
--
-- Nodes connected by a PHI node are considered equivalent.
-- Returns the graph and a mapping from references to vertexes.
mkEscapeGraph :: RecordState -> S.Set TRef -> EscapeGraph
mkEscapeGraph rs roots =
  --Tr.trace ("node_ids = " ++ show node_ids) $
  (buildG graph_range $ coll (S.toList roots) S.empty id, the_id)
 where
   graph_range = (1, rs_next rs - 1) :: (Int, Int)

   node_ids :: UArray Int Int
   node_ids =
     listArray graph_range $
       [ node_id n | n <- [fst graph_range .. snd graph_range] ]
   (ltr, rtl) = rs_phis rs
   node_id refi =
     let r1 = maybe refi refToInt $ IM.lookup refi ltr
         r2 = maybe refi refToInt $ IM.lookup refi rtl
     in refi `max` r1 `max` r2

   the_id ref =
     let !n = refToInt ref in
     if n > 0 then node_ids ! n else n
   
   coll [] _seen acc = acc []
   coll (ref:refs) !seen acc0
    | ref `S.member` seen = coll refs seen acc0
    | otherwise =
      -- We treat nodes that are connected via PHI nodes as identical.
      -- E.g., given node x with children x1 and x2, node y with children
      -- y1 and y2 and PHI(x,y) the graph will only contain nodes x, x1, x2.
      -- We will have `the_id x == the_id y`.
      let 
        iref = the_id ref
        children' = maybe [] (filter ((> 0) . the_id) . IM.elems . snd) $
                     IM.lookup iref (rs_virtual_objects rs)
        edges' = [ (iref, the_id r) | r <- children' ]
      in
        coll (children' ++ refs) (S.insert ref seen)
              ((edges'++) . acc0)
    

--filter_fold ::

--filter_cse = const (return (Left (\rs _ -> rs)))

--no_filter = const (return (Left (\rs _ -> rs)))



test_record1 :: FinalBCOs -> IO ()
test_record1 _bcos = return ()
{-  do
  let (itbls, heap) = loadBCOs bcos
  args <- mkArgStack
  rs_ref <- newIORef =<< mkRecordState
  _ <- record1 heap args (Mid (Assign (BcReg 1) (Move (BcReg 2)))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 1) (BinOp OpAdd Int32Ty (BcReg 1) (BcReg 2)))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 2) (Load (LoadLit (CInt 42))))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 1) (BinOp OpSub Int32Ty (BcReg 1) (BcReg 2)))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 2) (Load (LoadClosureVar 1)))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 3) (Load (LoadClosureVar 1)))) rs_ref
  _ <- record1 heap args (Mid (Assign (BcReg 4) (Load (LoadClosureVar 2)))) rs_ref
  pprint =<< ppRecordState =<< readIORef rs_ref -}
--  pprint (TRef 4)
