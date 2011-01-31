{-# LANGUAGE FlexibleContexts #-}
-- Source code from GHC  (BSD3)
-- Authors:  (c) Ben Lippmeier, Ian Lynagh, Simon Marlow

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
module Lambdachine.Utils.Graph.Ops where

import Lambdachine.Utils.Graph.Base
import Lambdachine.Utils.Unique
import Lambdachine.Utils.Pretty

import Data.Monoid
import Data.Maybe ( fromMaybe, catMaybes, isNothing )
import Data.List  ( foldl', sortBy, mapAccumL )
import Data.Foldable ( toList )
import Data.Ord   ( comparing )

-- | Lookup a node from the graph.
lookupNode :: Uniquable k =>
              k -> Graph k cls colour -> Maybe (Node k cls colour)
lookupNode k graph = lookupUM k (graphMap graph)

-- | Get a node from the graph, throwing an error if it's not there
getNode :: Uniquable k =>
           k -> Graph k cls colour -> Node k cls colour
getNode k graph = fromMaybe err (lookupNode k graph)
 where err = error "Graph.Ops.getNode: not found"

-- | Add a node to the graph, linking up its edges
addNode :: Uniquable k =>
           k -> Node k cls colour
        -> Graph k cls colour -> Graph k cls colour
addNode k node graph =
  graph{ graphMap = insertUM k node map_coalesce }
 where
   map_conflict =
     foldUS
       (adjustUM (\n -> n{ nodeConflicts =
                             insertUS k (nodeConflicts n) }))
       (graphMap graph)
       (nodeConflicts node)

   map_coalesce =
     foldUS
       (adjustUM (\n -> n{ nodeCoalesce =
                             insertUS k (nodeCoalesce n)}))
       map_conflict
       (nodeCoalesce node)

-- | Delete a node and all its edges from the graph.
delNode :: Uniquable k => k -> Graph k cls colour
        -> Maybe (Graph k cls colour)
delNode k graph | Just node <- lookupNode k graph =
  let
    -- delete conflict edges from other nodes to this one.
    graph1 =
      foldl' (\g k1 -> let Just g' = delConflict k1 k g in g') graph
	   $ toList (nodeConflicts node)

    -- delete coalesce edge from other nodes to this one.
    graph2 =
      foldl' (\g k1 -> let Just g' = delCoalesce k1 k g in g') graph1
	   $ toList (nodeCoalesce node)

    -- delete the node
    graph3 = graph2{ graphMap = deleteUM k (graphMap graph2) }
  in
    Just graph3

-- | Modify a node in the graph.
--
-- Returns 'Nothing' if the node isn't present.
--
modNode :: Uniquable k =>
           k
	-> (Node k cls colour -> Node k cls colour)
	-> Graph k cls colour -> Maybe (Graph k cls colour)
modNode k f graph =
  case lookupNode k graph of
    Nothing -> Nothing
    Just node ->
      let !node' = f node in
      Just graph{ graphMap = insertUM k node' (graphMap graph) }

-- | Size of the graph (number of nodes). O(n)
graphSize :: Graph k cls colour -> Int
graphSize graph = sizeUM (graphMap graph)

-- | Add a conflict between nodes in the graph.
--
-- A conflict requires that the corresponding nodes be coloured
-- differently.
--
-- Creates nodes if not present.
addConflict :: Uniquable k =>
               (k, cls) -> (k, cls)
            -> Graph k cls colour -> Graph k cls colour
addConflict (u1, c1) (u2, c2) graph =
  modifyGraphMap graph $ \mp ->
    addNeighbour u1 c1 u2 (addNeighbour u2 c2 u1 mp)
 where
   addNeighbour u c u' =
     insertWithUM (add_to_old_node u') u
                  (newNode u c){ nodeConflicts = singletonUS u' }

   add_to_old_node u' _new old =
     old{ nodeConflicts = insertUS u' (nodeConflicts old) }

-- | Add conflicts between given nodes.
--
-- Creates conflict edges between any two nodes in the set.
--
-- Creates nodes as necessary.
addConflicts :: forall k cls colour. Uniquable k =>
                UniqueSet k -> (k -> cls)
             -> Graph k cls colour -> Graph k cls colour
addConflicts conflictSet getClass graph
  | [u] <- conflicts
  = modifyGraphMap graph $ \mp ->
      adjustOrInsertUM id u (newNode u (getClass u)) mp
  | otherwise
  = modifyGraphMap graph $ \mp ->
      foldl' (\mp' u -> addConflictSet1 u mp') mp
             conflicts
 where
   conflicts = toList conflictSet

   addConflictSet1 :: k -> UniqueMap k (Node k cls colour)
                   -> UniqueMap k (Node k cls colour)
   addConflictSet1 u =
     let !set = deleteUS u conflictSet in
     adjustOrInsertUM
       (\node -> node{ nodeConflicts =
                         set `mappend` nodeConflicts node })
       u
       (newNode u (getClass u)){ nodeConflicts = set }

-- | Delete a conflict edge @k1 -> k2@.
--
-- Returns 'Nothing' if the node isn't in the graph.
delConflict :: Uniquable k =>
               k -> k
	    -> Graph k cls colour -> Maybe (Graph k cls colour)
delConflict k1 k2 =
  modNode k1 $ \node ->
    node{ nodeConflicts = deleteUS k2 (nodeConflicts node) }

-- | Add a coalescence edge to the graph, creating nodes if required.
--
-- A coalescence edge encourages to assign the same colour to the two
-- nodes.
addCoalesce :: Uniquable k =>
               (k, cls) -> (k, cls)
            -> Graph k cls colour -> Graph k cls colour
addCoalesce (u1, c1) (u2, c2) graph =
  modifyGraphMap graph $ \mp ->
    addNeighbour u1 c1 u2 (addNeighbour u2 c2 u1 mp)
 where
   addNeighbour u c u' =
     adjustOrInsertUM
       (\node ->
          node{ nodeCoalesce = insertUS u' (nodeCoalesce node) })
       u
       (newNode u c){ nodeCoalesce = singletonUS u' }

-- | Delete a coalescence edge (@k1 -> k2@) from the graph.
delCoalesce :: Uniquable k =>
               k -> k
	    -> Graph k cls colour -> Maybe (Graph k cls colour)
delCoalesce k1 k2 =
  modNode k1 $ \node ->
    node{ nodeCoalesce = deleteUS k2 (nodeCoalesce node) }
-- | Set the colour of the given node.
setColour :: Uniquable k => k -> colour
          -> Graph k cls colour -> Graph k cls colour
setColour k c graph =
  graph{ graphMap =
    adjustUM (\n -> n{ nodeColour = Just c }) k (graphMap graph) }

-- | Find all the nodes that match the predicate.
scanGraph :: Uniquable k =>
             Graph k cls colour -> (Node k cls colour -> Bool)
          -> [Node k cls colour]
scanGraph graph pred = filter pred (elementsUM (graphMap graph))

-- | Freeze one node in the graph.
--
-- This is for the iterative coalescer.  Look for a move related node
-- of low degree and freeze it.
--
-- We probably don't need to scan the whole graph looking for the node
-- of absolute lowest degree. Just sample the first few and choose the
-- one with the lowest degree out of those. Also, we don't make any
-- distinction between conflicts of different classes.. this is just a
-- heuristic, after all.
--
-- IDEA: freezing a node might free it up for Simplify.. would be good
-- to check for triv right here, and add it to a worklist if known
-- triv\/non-move nodes.
--
freezeOneInGraph :: (Uniquable k) =>
                    Graph k cls colour
                 -> (Graph k cls colour, Bool)
freezeOneInGraph graph =
  let candidates =
        sortBy (comparing numConflicts) $
        take 5 $  -- 5 isn't special, it's just a small number.
        scanGraph graph (not . nullUS . nodeCoalesce)
  in
    case candidates of
      -- there wasn't anything available to freeze
      [] -> (graph, False)
      -- we found something to freeze
      (n:_) -> (freezeNode (nodeId n) graph, True)

 where
   numConflicts n = sizeUS (nodeConflicts n)

-- | Freeze a node.
--
-- This is for the iterative coalescer.  By freezing a node we give up
-- on ever coalescing it.  Move all its coalesce edges into the frozen
-- set - and update back edges from other nodes.
--
freezeNode :: Uniquable k =>
     k                  -- ^ key of the node to freeze
  -> Graph k cls colour  -- ^ the graph
  -> Graph k cls colour  -- ^ graph with that node frozen
freezeNode k graph = modifyGraphMap graph $ \fm ->
  let
    -- freeze all the edges in the node to be frozen
    Just node = lookupUM k fm
    !node' = node{ nodeCoalesce = mempty }
    !fm1 = insertUM k node' fm

    -- update back edges pointing to this node
    freezeEdge k n =
      if memberUS k (nodeCoalesce n)  -- inefficient?
        then n{ nodeCoalesce = deleteUS k (nodeCoalesce n) }
        else n -- If the edge isn't actually in the coelesce set then just ignore it.

    !fm2 = foldUS (adjustUM (freezeEdge k)) fm1 (nodeCoalesce node)
  in
    fm2

-- | Do agressive coalescing on this graph.
--
-- Returns the new graph and the list of pairs of nodes that got
-- coalesced together.  for each pair, the resulting node will have
-- the least key and be second in the pair.
--
coalesceGraph :: (Uniquable k, Ord k, Eq cls, Pretty k, Pretty cls, Pretty colour) =>
     Bool -- ^ If True, coalesce nodes even if this might make the
          -- graph less colourable (aggressive coalescing)
  -> Triv k cls colour
  -> Graph k cls colour
  -> (Graph k cls colour, [(k, k)])
     -- ^ pairs of nodes that were coalesced, in the order that the
     -- coalescing was applied.

coalesceGraph aggressive triv graph =
  coalesceGraph' aggressive triv graph []

coalesceGraph' aggressive triv graph kkPairsAcc =
  let
    -- Find all the nodes that have coalescence edges
    cNodes = scanGraph graph (not . nullUS . nodeCoalesce)

    -- Build a list of pairs of keys for node's we'll try and
    -- coalesce.  Every pair of nodes will appear twice in this list.
    --
    --	ie [(k1, k2), (k2, k1) ... ]
    --
    -- This is ok, GrapOps.coalesceNodes handles this and it's
    -- convenient for build a list of what nodes get coalesced
    -- together for later on.
    --
    cList =
      [ (nodeId node1, k2)
	  | node1 <- cNodes
          , k2 <- toList $ nodeCoalesce node1 ]

    -- do the coalescing, returning the new graph and a list of pairs of keys
    --	that got coalesced together.
    (graph', mPairs)
      = mapAccumL (coalesceNodes aggressive triv) graph cList

	-- keep running until there are no more coalesces can be found
   in
     case catMaybes mPairs of
       [] ->
         (graph', reverse kkPairsAcc)
       pairs ->
         coalesceGraph' aggressive triv graph'
                        (reverse pairs ++ kkPairsAcc)

-- | Coalesce this pair of nodes unconditionally \/ agressively.
--
-- The resulting node is the one with the least key, or the
-- precoloured node.
--
-- Returns:
--
--  * @Just@ the pair of keys if the nodes were coalesced the second
--    element of the pair being the least one or the precoloured one.
--
--  * @Nothing@ if either of the nodes weren't in the graph
--
coalesceNodes :: (Uniquable k, Ord k, Eq cls, Pretty k, Pretty cls, Pretty colour) =>
     Bool -- ^ If True, coalesce nodes even if this might make the
          -- graph less colourable (aggressive coalescing)
  -> Triv  k cls colour
  -> Graph k cls colour
  -> (k, k) -- ^ keys of the nodes to be coalesced
  -> (Graph k cls colour, Maybe (k, k))

coalesceNodes aggressive triv graph (k1, k2)
  | (kMin, kMax) <- if k1 < k2 then (k1, k2) else (k2, k1)

    -- the nodes being coalesced must be in the graph
  , Just nMin <- lookupNode kMin graph
  , Just nMax <- lookupNode kMax graph

    -- can't coalesce conflicting modes
  , not $ memberUS kMin (nodeConflicts nMax)
  , not $ memberUS kMax (nodeConflicts nMin)

    -- can't coalesce the same node
  , nodeId nMin /= nodeId nMax

    -- At least one node must be uncoloured
  , isNothing (nodeColour nMin) || isNothing (nodeColour nMax)

  = coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax

   -- don't do the coalescing after all
  | otherwise
  = (graph, Nothing)

coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax
  -- sanity checks
  | nodeClass nMin /= nodeClass nMax
  = error "coalesceNodes: can't coalesce nodes of different classes."

--  | not (isNothing (nodeColour nMin) && isNothing (nodeColour nMax))
--  = error $ "coalesceNodes: can't coalesce coloured nodes.\n" ++
--           pretty (nMin, nMax)

  -- TODO: Does this break some invariant elsewhere?
  -- Let nMin be the coloured node (if any)
  | Nothing <- nodeColour nMin, Just _ <- nodeColour nMax
  = coalesceNodes_merge aggressive triv graph kMax kMin nMax nMin

  | otherwise
  = let
      -- the new node gets all the edges from its two components
      node =
	Node { nodeId = kMin
	     , nodeClass  = nodeClass nMin
	     , nodeColour = nodeColour nMin
	       -- nodes don't conflict with themselves..
	     , nodeConflicts =
	         deleteUS kMin $ deleteUS kMax $
                 nodeConflicts nMin `mappend` nodeConflicts nMax

	     , nodeExclusions =
                 nodeExclusions nMin `mappend` nodeExclusions nMax

	     , nodePreference =
                 nodePreference nMin ++ nodePreference nMax

	        -- nodes don't coalesce with themselves..
	     , nodeCoalesce =
                 deleteUS kMin $ deleteUS kMax $
	         nodeCoalesce nMin `mappend` nodeCoalesce nMax
             }
    in
      coalesceNodes_check aggressive triv graph kMin kMax node

coalesceNodes_check aggressive triv graph kMin kMax node
  -- Unless we're coalescing aggressively, if the result node is not
  -- trivially colourable then don't do the coalescing.
  | not aggressive
  , not $ triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)
  = (graph, Nothing)

  | otherwise
  = let -- delete the old nodes from the graph and add the new one
      Just graph1 = delNode kMax graph
      Just graph2 = delNode kMin graph1
      graph3 = addNode kMin node graph2
    in
      (graph3, Just (kMax, kMin))
