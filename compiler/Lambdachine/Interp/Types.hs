{-# LANGUAGE PatternGuards, MultiParamTypeClasses, FlexibleInstances #-}
module Lambdachine.Interp.Types where

import Lambdachine.Id
import Lambdachine.Grin.Bytecode
import Lambdachine.Utils

import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.IntSet as IS
import qualified Data.Set    as S
import Data.Array.IO
import Data.Array ( Array )
import Data.Bits  ( (.|.) )
import Data.IORef ( IORef )
import Data.List  ( foldl' )
import Data.Maybe ( fromMaybe )
import Data.Ord   ( comparing )
import Data.Generics.Uniplate.Direct

-- -------------------------------------------------------------------

-- * Interpreter Types

-- | A primitive value that can be loaded into a register.
data Val
  = Loc Int -- ^ Reference to a heap location.
  | SLoc Id -- ^ Reference to a static location.
  | ILoc InfoTableId
  | ValI Integer
  | ValS String
  | Undef
  deriving Eq

type BCO = BytecodeObject' FinalCode

-- | The dynamic call stack.
data ArgStack = ArgStack
  { argBase :: {-# UNPACK #-} !Int
  , argTop  :: {-# UNPACK #-} !Int
  , argVals :: IOArray Int Val
  }

data PC = PC !InfoTable {-# UNPACK #-} !Int
  deriving Eq

newtype InfoTableId = ItblId Id
  deriving (Eq, Ord)

mkItblId :: Id -> InfoTableId
mkItblId x =
  case idDetails x of
    DataConInfoTableId -> ItblId x
    InfoTableId -> ItblId x
    TopLevelId -> ItblId (mkInfoTableId (idName x))

instance Pretty InfoTableId where
  ppr (ItblId x) = ppr x

data Closure = Closure
  { cl_itbl   :: InfoTableId
  , cl_fields :: V.Vector Val
  }

data InfoTable
  = CodeInfoTable
      { itblId :: InfoTableId  -- See Note "InfoTableId"
      , itblClosureType :: ClosureType
      , itblCode :: FinalCode
      }
  | ConstrInfoTable
      { itblId :: InfoTableId  -- See Note "InfoTableId"
      , itblTag :: Int }

-- Note: "InfoTableId"
--
-- In the low-level implementation this is just the address of the
-- first instruction (or anything like it).

instance Eq InfoTable where
  i1 == i2 = itblId i1 == itblId i2

data ClosureType = CtFun Int | CtThunk | CtCAF

instance Pretty InfoTable where
  ppr (CodeInfoTable _ ct code) =
    ppr ct $+$ (indent 2 $ ppr code)
  ppr (ConstrInfoTable _ tag) =
    text "CONSTR" <> parens (text "tag:" <> ppr tag)

instance Pretty ClosureType where
  ppr (CtFun n) = text "FUN_" <> int n
  ppr CtThunk = text "THUNK"
  ppr CtCAF = text "CAF"

type InfoTables = M.Map InfoTableId InfoTable

lookupInfoTable :: InfoTables -> InfoTableId -> InfoTable
lookupInfoTable itbls itbl = fromMaybe err $ M.lookup itbl itbls
 where err = error $ "No info table for: " ++ pretty itbl

-- -------------------------------------------------------------------

data Heap = Heap { heapStaticArea :: M.Map Id Closure
                 , heapAllocArea  :: IM.IntMap Closure
                 , heapPtr        :: {-# UNPACK #-} !Int }

data VMState = VMState
  { vm_env   :: InfoTables
  , vm_heap  :: Heap
  , vm_stack :: ArgStack
  , vm_pc    :: PC
  , vm_mode  :: VMMode
  , vm_hotcounts :: M.Map InfoTableId Int
  , vm_hotfunc :: Int
  , vm_traces :: M.Map InfoTableId Trace
  }

data VMMode = InterpMode | TraceMode InfoTableId (IORef RecordState)

data RecordState = RecordState
  { rs_trace_root :: InfoTableId
  , rs_slots :: IM.IntMap TRef
  , rs_modified_slots :: IM.IntMap TRef
    -- ^ Maps register contents to their current 'TRef'.
  , rs_baseslot :: !Int
  , rs_topslot :: !Int
  , rs_trace :: IOArray Int IRIns
  , rs_next  :: !Int
  , rs_consts :: M.Map TConst TRef
  , rs_next_const :: !Int
  , rs_virtual_objects :: IM.IntMap (Int, IM.IntMap TRef)
  , rs_next_virt_obj :: !Int
  , rs_loop_ins :: !Int
  , rs_ins_to_ref :: IM.IntMap TRef
--  , rs_
  , rs_stackframes :: StackFrame
  , rs_escapees :: S.Set TRef
  , rs_equi_escape :: M.Map TRef TRef
  , rs_phis :: !(IM.IntMap TRef, IM.IntMap TRef)
  }

data Snapshot = Snapshot
  { snap_slots :: !(IM.IntMap TRef)
  , snap_base :: !Int
  , snap_top  :: !Int
  , snap_allocs :: S.Set TRef
  , snap_heap :: M.Map TRef (Int, IM.IntMap TRef)
  , snap_esc :: S.Set BcVar
  , snap_loc :: (InfoTable, Int)
  } 
 | ProtoSnapshot { snap_esc :: S.Set BcVar, snap_loc :: (InfoTable, Int) }
 deriving Eq
 -- TODO: rename snap_esc into snap_lives or snap_liveOuts

data Trace = Trace
  { tr_code :: V.Vector IRIns
  , tr_exits :: IM.IntMap Int  -- instruction |-> exit id
  }

-- class PrettyRS a where
--   pprRS :: RecordState -> a -> PDoc

-- instance PrettyRS TRef where
--   pprRS ref | isConstTRef ref =

-- | The intermediate representation used by the trace compiler.
data IRIns_ ref
  = Nop
    -- ^ No operation.  Replaces redundant instructions.
  | Loop
    -- ^ Placed after the loop header (the first loop iteration).  If
    -- present, the last trace instruction implicitly jumps back to
    -- this marker.
  | SLoad Int
    -- ^ Load a slot from the entry frame of the trace.
  | FLoad ref Int
    -- ^ Load a field with specified field id.
  | LoadBase Int  -- ref = &base[n]
  | SetBase Int  -- baseslot = n
  | Op BinOp ref ref
  | Guard CmpOp ref ref Snapshot
  | AllocN ref Int
  | FStore ref Int ref
  | UpdateR ref ref
  | Phi ref ref
  | PushFrame ref ref Int [Int]
    -- return address, node ptr, frame size, live-outs of current frame
  | PopFrame
  deriving (Eq)

type IRIns = IRIns_ TRef

data StackFrame
  = EntryFrame
  | UpdateFrame TRef StackFrame -- ret addr
  | ArgFrame TRef TRef Int (IM.IntMap TRef) StackFrame
    -- ret addr, node, framesize, live-outs

instance Uniplate TRef where uniplate = plate

instance Biplate (IRIns_ TRef) TRef where
  biplate (FLoad r n) = plate FLoad |* r |- n
  biplate (Op op l r) = plate (Op op) |* l |* r
  biplate (Guard cmp l r s) = plate (Guard cmp) |* l |* r |+ s
  biplate (FStore p n v) = plate FStore |* p |- n |* v
  biplate (AllocN i n) = plate AllocN |* i |- n
  biplate (UpdateR p q) = plate UpdateR |* p |* q
  biplate (PushFrame r p n ls) = plate PushFrame |* r |* p |- n |- ls
  biplate (Phi p q) = plate Phi |* p |* q
  biplate x = plate x

instance Biplate Snapshot TRef where
  biplate s =
    plate (\slots' heap' -> 
             s{ snap_slots = slots'
              , snap_heap = M.fromList heap' })
      |+ snap_slots s ||+ M.toAscList (snap_heap s)
--  biplate x = plate x

instance Biplate (IM.IntMap TRef) TRef where
  biplate m =
    plate IM.fromDistinctAscList ||+ IM.toAscList m

instance Biplate (TRef, (Int, IM.IntMap TRef)) TRef where
  biplate (r, rs) = plate (,) |* r |+ rs

instance Biplate (Int, IM.IntMap TRef) TRef where
  biplate (n, m) = plate ((,) n) |+ m

instance Biplate (Int, TRef) TRef where
  biplate (n, r) = plate ((,) n) |* r

--  | ConstI Integer


data TRef
  = TNil
  | TVar Int
  | TCst Int TConst
  | THp Int
  | TBase Int
--  deriving (Ord, Eq)

instance Eq TRef where
  x == y = refToInt x == refToInt y
{-
  TNil == TNil = True
  TCst n _ == TCst m _ = m == n
  TBase n == TBase m = n == m
  TVar n == TVar m = m == n
  TVar n == THp m  = m == n
  THp n  == TVar m = m == n
  THp n  == THp m  = m == n
  x == y = False
-}
instance Ord TRef where
  compare x y = comparing refToInt x y
{-
  TNil `compare` x = LT
  x `compare` TNil = GT
  TCst n _ `compare` TCst m _ = n `compare` m
  TCst _ _ `compare` x = LT
  x `compare` TCst _ _ = GT
  THp n  `compare` THp m  = n `compare` m
  THp n  `compare` TVar m = n `compare` m
  TVar n `compare` THp m  = n `compare` m
  TVar n `compare` TVar m = n `compare` m
  TBase m `compare` TBase n = m `compare` n
  TBase _ `compare` _ = GT
  _ `compare` TBase _ = LT
-}
refToInt :: TRef -> Int
refToInt TNil = minBound
refToInt (TCst n _) = (-n)
refToInt (THp n) = n
refToInt (TVar n) = n
refToInt (TBase n) = 0x1000000 .|. n

{-
newtype TRef = TRef Int
  -- positive: ref
  -- zero:     nil
  -- negative: const_ref
  deriving (Ord, Eq)
-}
nilTRef :: TRef
nilTRef = TNil

isVarTRef :: TRef -> Bool
isVarTRef (TVar _) = True
isVarTRef _ = False

isConstTRef :: TRef -> Bool
isConstTRef (TCst _ _) = True
isConstTRef _ = False

isHeapRef :: TRef -> Bool
isHeapRef (THp _) = True
isHeapRef _ = False

instance Pretty TRef where
  ppr (TVar n) = char '%' <> ppFill 4 n
  ppr TNil = text "-"
  ppr (TCst _ c) = ppr c
  ppr (THp n) = text "%*" <> ppFill 3 n
  ppr (TBase b) = text "%B" <> ppFill 3 b
{-  ppr (TRef n) | n > 0 = char '%' <> ppFill 4 n
               | n < 0 = text "%C" <> ppFill 3 (-n)
               | n == 0 = text "-"
-}
data TConst
  = TCInt Integer
  | TCStr String
  | TCId Id
  | TCItbl InfoTableId
  | TCPC InfoTableId Int
  deriving (Eq, Ord)

refConst :: TRef -> TConst
refConst (TCst _ c) = c

instance Pretty ref => Pretty (IRIns_ ref) where
  ppr Nop          = text "NOP     "
  ppr Loop         = text "--- LOOP ----------------------"
  ppr (LoadBase n) = text "BASE    " <+> ppr n
  ppr (SLoad n)    = text "LOADSLOT" <+> ppr n
  ppr (FLoad p o)  = text "LOADFLD " <+> ppr p <+> ppr o
  ppr (Op OpAdd l r)    = text "ADD     " <+> ppr l <+> ppr r
  ppr (Op OpSub l r)    = text "SUB     " <+> ppr l <+> ppr r
  ppr (Op OpMul l r)    = text "MUL     " <+> ppr l <+> ppr r
  ppr (Op OpDiv l r)    = text "DIV     " <+> ppr l <+> ppr r
  ppr (Op cmp l r)      = text "SET" <> ppr cmp <+> ppr l <+> ppr r
  ppr (Guard cmp l r s) = align ({-ppr s $+$-} pp_cmp cmp <+> ppr l <+> ppr r)
   where 
     pp_cmp CmpEq = text "EQ      "
     pp_cmp CmpNe = text "NE      "
     pp_cmp CmpLt = text "LT      "
     pp_cmp CmpLe = text "LE      "
     pp_cmp CmpGt = text "GT      "
     pp_cmp CmpGe = text "GE      "
  ppr (AllocN r n)      = text "(alloc) " <+> ppr r <+> ppr n
  ppr (FStore p n v)    = text "(stfld) " <+> ppr p <+> ppr n <+> ppr v
--  ppr (FStore p n v)    = text "STOREFLD" <+> ppr p <+> ppr n <+> ppr v
  ppr (UpdateR dst src)  = text "UPDATE  " <+> ppr dst <+> ppr src
  ppr (Phi x y)          = text "PHI     " <+> ppr x <+> ppr y
  ppr (PushFrame ret node sz lvs) =
    text "(frame) " <+> ppr ret <+> ppr node <+> ppr sz <+> 
         brackets (hsep (commaSep (map ppr lvs)))
  ppr PopFrame = text "(popfrm)"

opName :: BinOp -> String
opName OpAdd = "ADD"
opName OpSub = "SUB"
opName OpMul = "MUL"
opName OpDiv = "DIV"
opName CmpEq = "EQ"
opName CmpNe = "NE"
opName CmpLt = "LT"
opName CmpLe = "LE"
opName CmpGt = "GT"
opName CmpGe = "GE"

instance Pretty Snapshot where
  ppr (ProtoSnapshot esc _) =
    text "<proto-snap>, Esc=" <> braces (hsep (commaSep (map ppr (S.toList esc))))
  ppr snap = 
    indent 4 (pp_slots (snap_slots snap) <> char '}' $+$
                        pp_heap (M.toList $ snap_heap snap)) $+$
      text "Live=" <> braces (hsep (commaSep (map ppr (S.toList (snap_esc snap))))) $+$
      text "Sink=" <> braces (hsep (commaSep (map ppr (S.toList (snap_allocs snap)))))
   where
    pp_slots slots | IM.null slots = text "{"
    pp_slots slots =
     let slot_keys = IM.keysSet slots
         base = snap_base snap
         slot_low = IS.findMin slot_keys
         slot_hi  = IS.findMax slot_keys
     in collect' (text "Slots: {" <> ppr slot_low <> char ':')
                 [slot_low..slot_hi] $ \doc s ->
          doc <>
          (if s == base then char '|' else 
             if s `mod` 5 == 0 then char '.' else char ' ') <>
          (maybe (text "-----") ppr (IM.lookup s slots))

instance Pretty Trace where
  ppr (Trace irs_ _) =
    let inss = zip [1..] (V.toList irs_) in
    text "Trace {" 
    $+$ vcat (map (\(i, ins) -> ppFill 4 i <+> ppr ins) inss)
    $+$ text "}"

pp_heap :: Pretty a => [(a, (Int, IM.IntMap TRef))] -> PDoc
pp_heap objs =
     collect' (text "Heap") objs $ \doc (ptr, (sz, fields)) ->
       doc <+>
       ppr ptr <> (collect' (text "={") [0..sz-1] $ \d i ->
                     d <+> maybe (text "----") ppr (IM.lookup i fields))
               <+> char '}'

  
instance Pretty TConst where
  ppr (TCInt n) | n >= 0 = char '+' <> ppr n
                | otherwise = ppr n
  ppr (TCStr s) = text (show s)
  ppr (TCId x) = ppr x <> text "_clos"
  ppr (TCItbl x) = ppr x
  ppr (TCPC itbl_id n) = text "pc<"<> ppr itbl_id <> colon <> ppr n <> char '>'

isRecording :: VMState -> Bool
isRecording vm = case vm_mode vm of
                   InterpMode -> False
                   TraceMode _ _ -> True

lookupClosure :: Heap -> Val -> Closure
lookupClosure heap addr@(SLoc x)
 | Just cl <- M.lookup x (heapStaticArea heap) = cl
lookupClosure heap addr@(Loc l)
 | Just cl <- IM.lookup l (heapAllocArea heap) = cl
lookupClosure _ addr = invalidHeapAddressError addr

invalidHeapAddressError :: Val -> a
invalidHeapAddressError addr =
  error $ "lookupClosure: Invalid address " ++ pretty addr

updateClosure :: Heap -> Val -> Closure -> Heap
updateClosure heap addr new_cl = case addr of
  SLoc x -> heap{ heapStaticArea = M.alter f x (heapStaticArea heap) }
  Loc l -> heap{ heapAllocArea = IM.alter f l (heapAllocArea heap) }
 where
   f (Just _) = Just new_cl
   f Nothing = invalidHeapAddressError addr

allocClosure :: Heap -> Closure -> (Heap, Val)
allocClosure heap@Heap{ heapPtr = ptr } new_cl =
  (heap{ heapAllocArea = IM.insert ptr new_cl (heapAllocArea heap)
       , heapPtr = ptr + 1 }, Loc ptr)


instance Pretty Val where
  ppr (Loc p) = char '#' <> ppr p
  ppr (SLoc p) = char '#' <> ppr p
  ppr (ILoc i) = ppr i
  ppr (ValI n) = ppr n
  ppr (ValS s) = text (show s)
  ppr Undef = text "undef"


instance Pretty Closure where
  ppr (Closure info fields) =
    parens $ sep (ppr info : map ppr (V.toList fields))

instance Pretty Heap where
  ppr Heap{ heapStaticArea = s, heapAllocArea = m } =
    text "Static Closures:" $+$ indent 2 (ppr s) $+$
    text "Heap:" $+$ indent 2 (ppr m)

getField :: Closure -> Int -> Val
getField (Closure _ fields) field_id
  | field_id > 0, field_id <= V.length fields
  = fields V.! (field_id - 1)
getField obj field_id = invalidFieldIdError obj field_id

setField :: Closure -> Int -> Val -> Closure
setField (Closure dcon fields) field_id val
  | field_id > 0, field_id <= V.length fields
  = Closure dcon (fields V.// [(field_id - 1, val)])
setField obj field_id _ = invalidFieldIdError obj field_id

invalidFieldIdError :: Closure -> Int -> a
invalidFieldIdError obj field_id =
  error $ pretty $
    text "Invalid field id:" <+> ppr field_id <+>
    text "for object" <+> ppr obj

-- | Update contents of specified register in the current frame.
writeReg :: ArgStack -> Int -> Val -> IO ()
writeReg (ArgStack base top args) reg val
  | let reg_idx = base + reg, reg_idx < top = do
      writeArray args reg_idx val
writeReg _ reg _ =
  error $ "Register " ++ show reg ++ " not accessible in current frame."

-- | Read a register.  Register number must be in the range
-- @[(-1) .. framesize - 1]@.
--
-- Register @(-1)@ is the special @Node@ pointer which points to the
-- current closure and is used to access free variables.
readReg :: ArgStack -> Int -> IO Val
readReg (ArgStack base top args) reg
  | let reg_idx = base + reg, reg_idx < top, reg >= (-1) = do
      readArray args reg_idx
readReg _ reg =
  error $ "Register " ++ show reg ++ " not accessible in current frame."

alloc_static_closure :: Heap -> Id -> Closure -> Heap
alloc_static_closure heap x cl =
  heap{ heapStaticArea = M.insert x cl (heapStaticArea heap) }

loadBCOs :: FinalBCOs -> (InfoTables, Heap)
loadBCOs bcos =
  foldl' load1 (M.empty, Heap M.empty IM.empty 0) (M.toList bcos)
 where
--   load1 :: 
   load1 (itbls, heap) (x, BcConInfo{ bcoConTag = tag }) =
     (M.insert (ItblId x) (ConstrInfoTable (ItblId x) tag) itbls, heap)
   load1 (itbls, heap) (x, bco@BcoCon{}) =
     (itbls, alloc_static_closure heap x $
              Closure (ItblId (bcoDataCon bco))
                      (V.fromList (map (either constToVal SLoc)
                                       (bcoFields bco))))
   load1 (itbls, heap) (x, bco@BcObject{}) =
     let x' = ItblId (mkInfoTableId (idName x)) in
     (M.insert x' (CodeInfoTable x'
                             (closure_type (bcoType bco))
                             (bcoCode bco))
                itbls,
      alloc_static_closure heap x (Closure x' V.empty))

   closure_type (BcoFun arity) = CtFun arity
   closure_type Thunk = CtThunk
   closure_type CAF = CtCAF

mkArgStack :: IO ArgStack
mkArgStack = do
  argStack <- newArray (0, 4096) Undef
  return $ ArgStack 0 0 argStack

ppArgStack :: ArgStack -> Heap -> IO PDoc
ppArgStack (ArgStack base top vals) heap = do
  elems <- getElems vals
  let (saved, frame0) = splitAt base elems
      frame = take (top - base) frame0
  return $
    text "Stack" <+> ppr base <> char '-' <> ppr top <+>
      align (--fillSep (commaSep (map ppr saved)) <+> char '|' <+>
             fillSep (commaSep (map ppr frame)) $+$
             ppHeapFromRoots (last saved : frame) heap)

-- | Pretty-print transitive closure of heap values.
--
-- Follows all pointers starting from the roots, prints the objects
-- they point to, and then does the same for all pointers contained
-- in the printed objects.
--
-- Each object is printed only once.
ppHeapFromRoots :: [Val]  -- ^ Roots.  Non-pointer values are ignored.
                -> Heap -> PDoc
ppHeapFromRoots roots heap =
  fillSep $ commaSep $ go IS.empty [ l | Loc l <- roots ]
 where
   go seen [] = []
   go seen (l:ls)
     | l `IS.member` seen = go seen ls
   go seen (l:ls) =
     let obj@(Closure _ fields) = lookupClosure heap (Loc l) in
     (ppr (Loc l) <> char '=' <> ppr obj) :
        go (IS.insert l seen)
           ([ l' | Loc l' <- V.toList fields ] ++ ls)

-- TODO: should be stack-frame number of nested stack frames, not words.
stackDepth :: ArgStack -> Int
stackDepth (ArgStack base _ _) = base

constToVal :: BcConst -> Val
constToVal (CInt n) = ValI n
constToVal (CStr s) = ValS s

