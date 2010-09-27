{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
module Lambdachine.Interp.Trace
  ( IRIns(..), TRef, mkRecordState, record1, finaliseTrace,
    test_record1 )
where

import Lambdachine.Grin.Bytecode
import Lambdachine.Utils
import Lambdachine.Id
import Lambdachine.Interp.Types
import Lambdachine.Builtin

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Control.Monad ( forM_ )
import Data.Array.IO
import Data.IORef
import qualified Data.Vector as V

-- -------------------------------------------------------------------

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

setSlot :: IORef RecordState -> Int -> TRef -> IO ()
setSlot rs_ref slot ref = do
  modifyIORef rs_ref $ \rs ->
    let real_slot = slot + rs_baseslot rs in
    rs{ rs_slots = IM.insert real_slot ref (rs_slots rs) }

emitRaw :: IORef RecordState -> IRIns -> IO TRef
emitRaw rs_ref ir_ins = do
  rs <- readIORef rs_ref
  let ins_id = rs_next rs
  pprint $ ppr ins_id <+> ppr ir_ins
  writeArray (rs_trace rs) ins_id ir_ins
  let tref = TVar ins_id 
  writeIORef rs_ref $! rs{ rs_next = 1 + ins_id }
  return tref

emit :: IORef RecordState -> InfoTables -> Heap -> IRIns -> IO TRef
emit rs env heap ins =
  case ins of
    Guard CmpEq x y
      | x == y -> do
        putStrLn $ "!!! Skipping constant guard"
        return nilTRef
    FLoad x offs
      | isConstTRef x, TCId addr <- refConst x, offs == 0 -> do
          let Closure itbl_id _ = lookupClosure heap (SLoc addr)
          loadConst rs (TCItbl itbl_id)
        -- Not sure how to optimise other fields yet.
    _ ->
      emitRaw rs ins
     
  
ppSlots :: IORef RecordState -> IO PDoc
ppSlots rs_ref = do
  rs <- readIORef rs_ref
  return $ pp_slots rs (rs_slots rs) <> char '}'
 where
   pp_slots rs slots | IM.null slots = text "{"
   pp_slots rs slots =
     let slot_keys = IM.keysSet slots
         base = rs_baseslot rs
         slot_low = min (IS.findMin slot_keys) base
         slot_hi  = max (IS.findMax slot_keys) base
     in collect' (text "Slots: {" <> ppr slot_low <> char ':')
                 [slot_low..slot_hi] $ \doc s ->
          doc <>
          (if s == base then char '|' else 
             if s `mod` 5 == 0 then char '.' else char ' ') <>
          (case IM.lookup s slots of
             Nothing -> text "-----"
             Just t -> ppr t)
  
mkRecordState :: IO (IORef RecordState)
mkRecordState = do
  trace <- newArray_ (1, 2000)
  newIORef $
    RecordState{ rs_slots = IM.empty
               , rs_baseslot = 0
               , rs_trace = trace
               , rs_next = 1
               , rs_consts = M.empty
               , rs_next_const = 1 }

ppRecordState :: RecordState -> IO PDoc
ppRecordState rs = do
  inss <- take (rs_next rs - 1) `fmap` getAssocs (rs_trace rs)
  return $ --ppr (rs_consts rs) $+$
           vcat (map (\(i, ins) -> ppFill 4 i <+> ppr ins) inss) $+$
           ppr (rs_slots rs)

-- getSlot :: IORef RecordState -> Int -> IO TRef
-- getSlot rs_ref reg = do
--   insertIRFiltered rs_ref (SLoad reg) $ \rs ->
--     case IM.lookup reg (rs_slots rs) of
--       Just tref -> return (Right (tref, rs))
--       Nothing ->
--         return $ Left $ \rs' tref ->
--           rs'{ rs_slots = IM.insert reg tref (rs_slots rs') }

-- setSlot :: IORef RecordState -> Int -> TRef -> IO ()
-- setSlot rs_ref reg tref =
--   modifyIORef rs_ref $ \rs ->
--     rs{ rs_slots = IM.insert reg tref (rs_slots rs) }

loadConst :: IORef RecordState -> TConst -> IO TRef
loadConst rs_ref bc_const = do
  rs <- readIORef rs_ref
  case M.lookup bc_const (rs_consts rs) of
    Just tref -> return tref
    Nothing -> do
      let tref = TCst (rs_next_const rs) bc_const
      writeIORef rs_ref $! rs{ rs_next_const = 1 + rs_next_const rs
                             , rs_consts = M.insert bc_const tref (rs_consts rs) }
      pprint $ text "Const" <+> ppr tref <+> char '=' <+> ppr bc_const
      return tref

insertIRFiltered :: IORef RecordState
                 -> IRIns
                 -> (RecordState -> IO (Either (RecordState -> TRef -> RecordState)
                                               (TRef, RecordState)))
                 -> IO TRef
insertIRFiltered rs_ref ins filter_fn = do
  rs <- readIORef rs_ref
  r <- filter_fn rs
  case r of
    Right (tref, rs') -> (writeIORef rs_ref $! rs') >> return tref
    Left upd -> do
      let ins_id = rs_next rs
      pprint $ ppr ins_id <+> ppr ins
      writeArray (rs_trace rs) ins_id ins
      let tref = TVar ins_id 
      writeIORef rs_ref $! upd rs{ rs_next = 1 + ins_id} tref
      return tref

record1 :: InfoTableId
        -> InfoTables -> Heap -> ArgStack -> InfoTable -> Int 
        -> FinalIns -> IORef RecordState
        -> IO Bool -- ^ Keep recording?
record1 root env heap args itbl pc ins rs = do
  pprint =<< ppSlots rs
  case ins of
    Mid (Assign (BcReg r) rhs) -> record_rhs r rhs
    Lst (Eval pc' (BcReg reg)) -> record_eval env heap args itbl pc reg rs
    Lst (Ret1 (BcReg reg)) -> record_return reg
    Lst (Call Nothing f params) -> record_tailcall f params
    Lst (Case CaseOnTag (BcReg reg) alts) -> record_case reg alts
    Lst Update -> record_update
 where
   record_rhs dst (Move (BcReg src)) = do
     ref <- getSlot rs src
     setSlot rs dst ref
     return True
   record_rhs dst (BinOp op ty (BcReg src1) (BcReg src2)) = do
     tref1 <- getSlot rs src1
     tref2 <- getSlot rs src2
     tref3 <- emit rs env heap (Op op tref1 tref2)
     --tref3 <- insertIRFiltered rs (Op op aref1 tref2) filter_cse
     setSlot rs dst tref3
     return True
   record_rhs dst (Load (LoadLit l)) = do
     tref <- loadConst rs (bc_const_to_tconst l)
     setSlot rs dst tref
     return True
   record_rhs dst (Load (LoadGlobal x)) = do
     tref <- if isDataConInfoTableId x
              then loadConst rs (TCItbl (ItblId x))
              else loadConst rs (TCId x)
     setSlot rs dst tref
     return True
   record_rhs dst (Load (LoadClosureVar offs)) = do
     node_ptr <- getSlot rs (-1)
     tref <- emit rs env heap (FLoad node_ptr offs)
     --tref <- insertIRFiltered rs (FLoad node_ptr offs) filter_cse
     setSlot rs dst tref
     return True
   record_rhs dst (Fetch (BcReg ptr) offs) = do
     tref <- getSlot rs ptr
     tref' <- emit rs env heap (FLoad tref offs)
     --tref' <- insertIRFiltered rs (FLoad tref offs) filter_cse
     setSlot rs dst tref'
     return True
   record_rhs dst (Alloc f vars) = do
     let nvars = length vars
     ptr <- emitRaw rs (AllocN (1 + nvars))
     forM_ (zip [0..nvars] (f:vars)) $ \(i, (BcReg r)) -> do
       ref <- getSlot rs r
       emitRaw rs (FStore ptr i ref)
     setSlot rs dst ptr
     return True

   record_tailcall (BcReg fn) params = do
     fnptr <- readReg args fn
     let Closure itbl_id _ = lookupClosure heap fnptr
     -- TODO: abort if back at root
     
     fnref <- getSlot rs fn
     itbl_ref <- emit rs env heap (FLoad fnref 0)  -- get info table
     expected_itbl <- loadConst rs (TCItbl itbl_id)
     emit rs env heap (Guard CmpEq itbl_ref expected_itbl)
     setSlot rs (-1) fnref
     refs <- mapM (\(BcReg r) -> getSlot rs r) params
     forM_ (zip [0..] refs) $ \(i, ref) -> setSlot rs i ref
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
         itbl = lookupInfoTable env itbl_id

     expected_ret_addr <- loadConst rs (TCPC itbl_id old_pc)
     actual_ret_addr <- getSlot rs (-2)
     emit rs env heap (Guard CmpEq actual_ret_addr expected_ret_addr)

     result_ref <- getSlot rs rslt
     let dst :: Int 
         dst =
           case fc_code (itblCode itbl) V.! (old_pc - 1) of
             Lst (Call (Just (BcReg dst, _)) _ _) -> dst
             Lst (Eval _ (BcReg dst)) -> dst
     modifyIORef rs $ \rs' ->
       rs'{ rs_baseslot = rs_baseslot rs' - 3 - fc_framesize (itblCode itbl) }
     setSlot rs dst result_ref
     return True

   record_update = do
     new_ptr <- getSlot rs 1
     old_ptr <- getSlot rs 0
     -- TODO: load indirection Itbl
     emitRaw rs (FStore old_ptr 0 new_ptr)
     record_return 1

   record_case reg _alts = do
     -- We don't actually need to look at the alternatives, because
     -- the interpreter will do the selection and then call record
     -- on the appropriate instruction.  We just have to emit the
     -- proper guard here.

     ptr <- readReg args reg
     let Closure itbl_id _ = lookupClosure heap ptr
     
     -- Emit a guard on the info table pointer
     ref <- getSlot rs reg
     itbl_ref <- emit rs env heap (FLoad ref 0)
     expected_itbl <- loadConst rs (TCItbl itbl_id)
     emit rs env heap (Guard CmpEq itbl_ref expected_itbl)
     return True
     
   bc_const_to_tconst (CInt n) = TCInt n
   bc_const_to_tconst (CStr s) = TCStr s
   
record_eval :: InfoTables -> Heap -> ArgStack -> InfoTable -> Int
            -> Int -> IORef RecordState -> IO Bool
record_eval env heap args itbl pc reg rs = do
  -- We specialise on the info table, hence we need to get its actual
  -- runtime value.
  ptr <- readReg args reg
  let Closure itbl_id _ = lookupClosure heap ptr
  
  -- Now emit the trace code and guard.  Note: field 0 = info table
  tref <- getSlot rs reg
  itblref <- insertIRFiltered rs (FLoad tref 0) filter_cse
  -- load the current info table as a constant
  expected_itbl <- loadConst rs (TCItbl itbl_id)
  insertIRFiltered rs (Guard CmpEq itblref expected_itbl) no_filter

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
        (itbl', pc') <- allocStackFrame rs env itbl pc upd_closure (ItblId updateItblId) [node_ptr]
        _ <- allocStackFrame rs env itbl' pc' tref (itblId obj) []
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
        
allocStackFrame :: IORef RecordState 
                -> InfoTables -- To look things up
                -> InfoTable -- Current info table
                -> Int  -- Current PC
                -> TRef  -- The function we're calling
                -> InfoTableId -- its expected info table (used for
                               -- setting up the stack frame
                -> [TRef] -- Arguments
                -> IO (InfoTable, Int)
allocStackFrame rs itbls itbl pc node_ref node_id args = do
  let top = fc_framesize (itblCode itbl)
  base_depth <- rs_baseslot `fmap` readIORef rs
  base_ref <- emitRaw rs (LoadBase base_depth)
  setSlot rs top base_ref
  pc_ref <- loadConst rs (TCPC (itblId itbl) (pc + 1))
  setSlot rs (top + 1) pc_ref
--  node_ref <- loadConst rs (TC node)
  setSlot rs (top + 2) node_ref
  let itbl@CodeInfoTable{} = lookupInfoTable itbls node_id
  --    framesize = fc_framesize code
  forM_ (zip [top + 3..] args) $ \(i, arg) -> setSlot rs i arg
  modifyIORef rs $ \rs' ->
    rs'{ rs_baseslot = rs_baseslot rs' + top + 3 }
  return (itbl, 0)

finaliseTrace :: IORef RecordState -> IO ()
finaliseTrace rs_ref = do
  pprint =<< ppRecordState =<< readIORef rs_ref

--filter_fold :: 

filter_cse = const (return (Left (\rs _ -> rs)))

no_filter = const (return (Left (\rs _ -> rs)))

test_record1 :: FinalBCOs -> IO ()
test_record1 bcos = return ()
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
