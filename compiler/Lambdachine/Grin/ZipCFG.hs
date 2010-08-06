{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambdachine.Grin.ZipCFG where

import Lambdachine.Utils.Unique
import Lambdachine.Utils.UniqueMap
import Lambdachine.Utils.UniqueSet
import Lambdachine.Utils.Pretty

import qualified Data.IntMap as IM
import Data.Supply
import Control.Applicative

--data Instr = Instr

newtype BlockId = BlockId Unique
  deriving (Eq, Ord)

instance Pretty BlockId where
  ppr (BlockId u) = text ".lcl_" <> ppr u

instance Uniquable BlockId where
  getUnique (BlockId u) = u

mkBlockIdSupply :: IO (Supply BlockId)
mkBlockIdSupply =
  (`modifySupply` f) <$> newUniqueSupply 'B'
 where
   f s = BlockId (supplyValue s)

newtype BlockEnv a = BlockEnv (UniqueMap a)

emptyBlockEnv :: BlockEnv a
emptyBlockEnv = BlockEnv emptyUM

singletonBlockEnv :: BlockId -> a -> BlockEnv a
singletonBlockEnv bid a = BlockEnv (singletonUM bid a)

lookupBlockEnv :: BlockEnv a -> BlockId -> Maybe a
lookupBlockEnv (BlockEnv blocks) b = lookupUM blocks b

insertBlockEnv :: BlockEnv a -> BlockId -> a -> BlockEnv a
insertBlockEnv (BlockEnv blocks) bid a =
  BlockEnv (insertUM bid a blocks)

deleteBlockEnv :: BlockEnv a -> BlockId -> BlockEnv a
deleteBlockEnv (BlockEnv blocks) bid =
  BlockEnv (deleteUM bid blocks)

--newtype Label = Label Unique
{-
data First
  = Entry
  | Label {-# UNPACK #-} !Unique
          {-# UNPACK #-} !Bool
-}        
--newtype ZMiddle i = Middle i

{-
data Last i r
  = Exit
  | Branch i BlockId
  | CBranch i Label Label
  | Case i [Label]
  | Return i [r]
-}

data ZLast l = LastExit
             | LastOther l

-- | A (reversed) sequence of middle nodes labelled by a 'BlockId'.
data ZHead middle
  = ZFirst BlockId
  | ZHead (ZHead middle) middle

-- | A sequence of middle nodes followed by a last node.
data ZTail middle last
  = ZLast (ZLast last)
  | ZTail middle (ZTail middle last)

data Block middle last = Block
  { block_id     :: BlockId
  , block_instrs :: ZTail middle last }

data Graph middle last = Graph
  { g_entry :: BlockId
--  , g_block_ids :: Supply Unique
  , g_blocks :: BlockEnv (Block middle last)
  }

data ZBlock middle last =
  ZBlock (ZHead middle) (ZTail middle last)

-- | A focused graph.
--
-- Note: The focused graph block ('fg_focus') is /not/ part of the
-- blocks `fg_others`.
data FGraph middle last = FGraph
  { fg_entry  :: BlockId
--  , fg_block_ids :: Supply Unique
  , fg_focus  :: ZBlock middle last
  , fg_others :: BlockEnv (Block middle last) }

class HasSuccessors l where
  succs :: l -> [BlockId]
  fold_succs :: (BlockId -> a -> a) -> l -> a -> a
  fold_succs add l z = foldr add z $ succs l

class HasSuccessors l => LastNode l where
  mkBranchNode :: BlockId -> l
  isBranchNode :: l -> Bool
  branchNodeTarget :: l -> BlockId

instance HasSuccessors l => HasSuccessors (ZLast l) where
  succs LastExit = []
  succs (LastOther l) = succs l
  fold_succs _ LastExit z = z
  fold_succs f (LastOther l) z = fold_succs f l z

instance LastNode l => LastNode (ZLast l) where
  mkBranchNode bid = LastOther (mkBranchNode bid)
  isBranchNode LastExit = False
  isBranchNode (LastOther l) = isBranchNode l
  branchNodeTarget LastExit = error "branchNodeTarget LastExit"
  branchNodeTarget (LastOther l) = branchNodeTarget l

instance LastNode l => HasSuccessors (ZBlock m l) where
  succs b = succs (getLast b)

instance LastNode l => HasSuccessors (Block m l) where
  succs b = succs (unzipBlock b)

instance LastNode l => HasSuccessors (ZTail m l) where
  succs b = succs (lastTail b)

-- -------------------------------------------------------------------

mkGraph :: BlockId -> FGraph m l
mkGraph bid =
  FGraph bid (ZBlock (ZFirst bid) (ZLast LastExit)) emptyBlockEnv

focus :: BlockId -> Graph m l -> FGraph m l
focus bid (Graph entry_id blocks) =
  case lookupBlockEnv blocks bid of
    Just blk ->
      FGraph entry_id (unzipBlock blk) (deleteBlockEnv blocks bid)
    Nothing ->
      error "focus called on non-existent block."

entry :: Graph m l -> FGraph m l
entry g@(Graph entry_id _) = focus entry_id g

--finish :: FGraph m l -> 

unzipBlock :: Block m l -> ZBlock m l
unzipBlock (Block bid tl) = ZBlock (ZFirst bid) tl

zipBlock :: ZBlock m l -> Block m l
zipBlock (ZBlock h t) = build_block h t

build_block (ZFirst bid) tl = Block bid tl
build_block (ZHead h m) tl  = build_block h (ZTail m tl)

addInstr :: m -> FGraph m l -> FGraph m l
addInstr m (FGraph entry (ZBlock hd tl) blocks) =
  FGraph entry (ZBlock (ZHead hd m) tl) blocks

getLast :: ZBlock m l -> ZLast l
getLast (ZBlock _ t) = lastTail t

lastTail :: ZTail m l -> ZLast l
lastTail (ZLast l) = l
lastTail (ZTail _ t) = lastTail t

closeBlock :: l -> FGraph m l -> FGraph m l
closeBlock l (FGraph entry (ZBlock hd tl) blocks) =
  let blk = build_block hd (ZLast (LastOther l))
      err = error "No current label.  Emit a label after 'closeBlock'"
  in FGraph entry err $! insertBlockEnv blocks (block_id blk) blk

finish :: FGraph m l -> Graph m l
finish (FGraph entry zblk blocks) =
  let blk = zipBlock zblk in
  Graph entry (insertBlockEnv blocks (block_id blk) blk)

addLabel :: BlockId -> FGraph m l -> FGraph m l
addLabel label (FGraph entry _ blocks) =
  FGraph entry (ZBlock (ZFirst label) (ZLast LastExit)) blocks

emptyGraph :: BlockId -> Graph m l
emptyGraph entry = Graph entry (singletonBlockEnv entry blk0)
 where blk0 = Block entry (ZLast LastExit)

-- -------------------------------------------------------------------
{-
newtype Reg = Reg Int

data Middle
  = MidMove Reg Reg
  | MidLit Reg Int
  | MidBin Reg Op Reg Reg

data Last
  = LastGoto BlockId
  | LastCondBranch Cond Reg Reg BlockId BlockId
  | LastSwitch Reg [(Int, BlockId)]
  | LastCall (Maybe Reg) Reg [Reg] (Maybe BlockId)

data Op = Add | Mul | Sub
data Cond = Lt | Gt | Eq

instance Pretty Reg where
  ppr (Reg n) = char 'r' <> int n

instance Pretty Op where
  ppr Add = char '+'
  ppr Mul = char '*'
  ppr Sub = char '-'

instance Pretty Cond where
  ppr Lt = char '<'
  ppr Gt = char '>'
  ppr Eq = char '='

instance Pretty Middle where
  ppr (MidMove dst src) = ppr dst <+> text "<-" <+> ppr src
  ppr (MidLit dst n) = ppr dst <+> text "<-" <+> int n
  ppr (MidBin dst op s1 s2) =
    ppr dst <+> text "<-" <+> ppr s1 <+> ppr op <+> ppr s2

instance Pretty Last where
  ppr (LastGoto bid) = text "goto" <+> ppr bid
  ppr (LastCondBranch c r1 r2 true_id false_id) =
    text "if" <+> ppr r1 <+> ppr c <+> ppr r2 <+> text "then goto"
      <+> ppr true_id
  ppr (LastSwitch r alts) =
    text "switch" <+> ppr r <+> align (cat (map pp_alt alts))
   where
     pp_alt (n, bid) = int n <> colon <+> ppr bid

instance LastNode Last where
  mkBranchNode = LastGoto
  isBranchNode (LastGoto _) = True
  isBranchNode _ = False
  branchNodeTarget (LastGoto bid) = bid

instance HasSuccessors Last where
  succs (LastGoto bid) = [bid]
  succs (LastCondBranch _ _ _ t f) = [f, t]
  succs (LastSwitch _ alts) = map snd alts

tst1 = do
  (l1:l2:l3:l4:_) <- map supplyValue . split <$> mkBlockIdSupply
  let g0 = emptyGraph l1 :: Graph Middle Last
  let fg1 = entry g0
      fg2 = addInstr (MidLit (Reg 1) 42) fg1
      fg3 = closeBlock (mkBranchNode l2) fg2
      fg4 = addLabel l2 fg3
      g = finish fg4 -- $ closeBlock (mkBranchNode l1) fg2
  pprint g

-}

-- | Return a list of blocks reachable from the entry node.
--
-- This list has the following property: Say a \"back reference\ "
-- exists if one of a block's control-flow successors precedes it in
-- the output list.  Then there are as few back references as
-- possible.
--
-- The output is suitable for use in a forward dataflow problem.  For
-- a backward problem, simply reverse the list.  ('postorder_dfs' is
-- sufficiently tricky to implement that one doesn't want to try and
-- maintain both forward and backward versions.)
--
-- Here's an easy way to go wrong!  Consider
-- 
-- @
--	A -> [B,C]
--	B -> D
--	C -> D
-- @
--
-- Then ordinary dfs would give [A,B,D,C] which has a back ref from C to D.
-- Better to get [A,B,C,D]
--
postorderDfs :: LastNode l => Graph m l -> [Block m l]
postorderDfs g@(Graph _ blockenv) =
  let FGraph id eblock _ = entry g in
  zipBlock eblock : 
    postorder_dfs_from_except blockenv eblock (unitBlockSet id)

type BlockSet = UniqueSet Unique

unitBlockSet x = singletonUS (getUnique x)
emptyBlockSet = emptyUS
elemBlockSet x bs = elemUS (getUnique x) bs
extendBlockSet bs x = insertUS (getUnique x) bs

postorder_dfs_from_except :: (HasSuccessors b, LastNode l)
                          => BlockEnv (Block m l) -> b -> BlockSet -> [Block m l]
postorder_dfs_from_except blocks b visited =
  vchildren (get_children b) (\acc _visited -> acc) [] visited
  where
    -- vnode ::
    --    Block m l -> ([Block m l] -> BlockSet -> a) -> [Block m l] -> BlockSet -> a
    vnode block@(Block id _) cont acc visited =
        if elemBlockSet id visited then
            cont acc visited
        else
            let cont' acc visited = cont (block:acc) visited in
            vchildren (get_children block) cont' acc (extendBlockSet visited id)
    vchildren bs cont acc visited =
        let next children acc visited =
                case children of []     -> cont acc visited
                                 (b:bs) -> vnode b (next bs) acc visited
        in next bs acc visited
    get_children block = foldl add_id [] (succs block)
    add_id rst id = case lookupBlockEnv blocks id of
                      Just b -> b : rst
                      Nothing -> rst

postorder_dfs_from :: (HasSuccessors b, LastNode l) =>
                      BlockEnv (Block m l)
                   -> b -> [Block m l]
postorder_dfs_from blocks b =
  postorder_dfs_from_except blocks b emptyBlockSet

-- * Pretty Printing

instance (Pretty m, Pretty l) => Pretty (Block m l) where ppr = pprBlock
instance Pretty l => Pretty (ZLast l) where
  ppr LastExit = text "<exit>"
  ppr (LastOther l) = ppr l

instance (Pretty m, Pretty l, LastNode l) => Pretty (Graph m l) where
  ppr = pprGraph

pprTail :: (Pretty m, Pretty l) => ZTail m l -> PDoc
pprTail (ZTail m t) = ppr m $$ pprTail t
pprTail (ZLast l) = ppr l

pprBlock :: (Pretty m, Pretty l) => Block m l -> PDoc
pprBlock (Block bid tl) =
  ppr bid <> colon $$ indent 3 (pprTail tl)

pprGraph :: (Pretty m, Pretty l, LastNode l) =>
            Graph m l -> PDoc
pprGraph g =
  char '{' $$ indent 2 (vcat (map ppr blocks)) $$ char '}'
 where blocks = postorderDfs g
