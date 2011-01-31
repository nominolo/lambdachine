-- Source code from GHC  (BSD3)
-- Authors:  (c) Ben Lippmeier, Ian Lynagh, Simon Marlow

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
module Lambdachine.Utils.Graph.Colour where

import Lambdachine.Utils.Graph.Base
import Lambdachine.Utils.Graph.Ops
import Lambdachine.Utils.Unique
import Lambdachine.Utils.Pretty

import Data.Foldable ( toList )
import Data.List ( mapAccumL )
import Data.Maybe ( catMaybes )

-- | Try to colour a graph with this set of colours.
-- 
-- Uses Chaitin's algorithm to colour the graph.  The graph is scanned
-- for nodes which are deemed 'trivially colourable'. These nodes are
-- pushed onto a stack and removed from the graph.  Once this process
-- is complete the graph can be coloured by removing nodes from the
-- stack (ie in reverse order) and assigning them colours different to
-- their neighbors.
--
colourGraph ::
     (Uniquable k, Uniquable cls, Uniquable colour,
      Eq colour, Eq cls, Ord k,
      Pretty k, Pretty cls, Pretty colour) =>
     Bool -- ^ whether to do iterative coalescing
  -> Int
     -- ^ how many times we've tried to colour this graph so far.
  -> UniqueMap cls (UniqueSet colour)
     -- ^ Map of (node class -> set of colours available for this
     -- class).
  -> Triv k cls colour
     -- ^ Function to decide whether a node is trivially colourable.
  -> (Graph k cls colour -> k)
     -- ^ Function to choose a node to potentially leave uncoloured if
     -- nothing is trivially colourable.
  -> Graph  k cls colour
     -- ^ The graph to colour.
     
  -> ( Graph k cls colour, UniqueSet k, UniqueMap k k )
     -- ^ Returns:
     --
     --  * the coloured graph
     --
     --  * the set of nodes that we couldn't find a colour for.
     --
     --  * map of regs @r1 -> r2@ that were coalesced; @r1@ should be
     --    replaced by @r2@ in the source.

colourGraph iterative spinCount colours triv spill graph0 =
  let
    -- If we're not doing iterative coalescing then do an aggressive
    -- coalescing first time around and then conservative coalescing
    -- for subsequent passes.
    --
    -- Aggressive coalescing is a quick way to get rid of many reg-reg
    -- moves. However, if there is a lot of register pressure and we
    -- do it on every round then it can make the graph less colourable
    -- and prevent the algorithm from converging in a sensible number
    -- of cycles.
    (graph_coalesced, kksCoalesce1)
      | iterative      = (graph0, [])
      | spinCount == 0 = coalesceGraph True  triv graph0
      | otherwise      = coalesceGraph False triv graph0

    -- run the scanner to slurp out all the trivially colourable nodes
    -- (and do coalescing if iterative coalescing is enabled)
    (ksTriv, ksProblems, kksCoalesce2) =
      colourScan iterative triv spill graph_coalesced

    -- If iterative coalescing is enabled, the scanner will coalesce
    -- the graph as does its business.  We need to apply all the
    -- coalescences found by the scanner to the original graph before
    -- doing assignColours.
    --
    -- Because we've got the whole, non-pruned graph here we turn on
    -- aggressive coalescing to force all the (conservative)
    -- coalescences found during scanning.
    -- 
    (graph_scan_coalesced, _) =
      mapAccumL (coalesceNodes True triv) graph_coalesced kksCoalesce2

    -- Colour the trivially colourable nodes.
    -- 
    -- During scanning, keys of triv nodes were added to the front of
    -- the list as they were found this colours them in the reverse
    -- order, as required by the algorithm.
    (graph_triv, ksNoTriv) =
       assignColours colours graph_scan_coalesced ksTriv

    -- Try and colour the problem nodes.
    -- 
    -- Problem nodes are the ones that were left uncoloured because
    -- they weren't triv.  theres a change we can colour them here
    -- anyway.
    (graph_prob, ksNoColour) =
       assignColours colours graph_triv ksProblems
                    
    -- if the trivially colourable nodes didn't colour then something is
    -- probably wrong with the provided triv function.

  in
    if not $ null ksNoTriv
      then
        error "colourGraph: trivially colourable nodes didn't colour!" -- empty
	  {- (  empty
		$$ text "ksTriv    = " <> ppr ksTriv
		$$ text "ksNoTriv  = " <> ppr ksNoTriv
		$$ text "colours    = " <> ppr colours
		$$ empty
		$$ dotGraph (\_ -> text "white") triv graph_triv) -}

      else
	( graph_prob
	, fromListUS ksNoColour -- the nodes that didn't colour (spills)
	, if iterative
	    then fromListUM kksCoalesce2
	    else fromListUM kksCoalesce1)


-- | Scan through the conflict graph separating out trivially
-- colourable and potentially uncolourable (problem) nodes.
--
-- Checking whether a node is trivially colourable or not is a
-- resonably expensive operation, so after a triv node is found and
-- removed from the graph it's no good to return to the 'start' of the
-- graph and recheck a bunch of nodes that will probably still be
-- non-trivially colourable.
--
-- To ward against this, during each pass through the graph we collect
-- up a list of triv nodes that were found, and only remove them once
-- we've finished the pass. The more nodes we can delete at once the
-- more likely it is that nodes we've already checked will become
-- trivially colourable for the next pass.
--
-- TODO: add work lists to finding triv nodes is easier.  If we've
-- just scanned the graph, and removed triv nodes, then the only nodes
-- that we need to rescan are the ones we've removed edges from.
--
colourScan ::
     (Uniquable k, Uniquable cls, Uniquable colour,
      Ord k, Eq cls, Pretty k, Pretty cls) =>
     Bool -- ^ whether to do iterative coalescing
  -> Triv k cls colour		-- ^ fn to decide whether a node is trivially colourable
  -> (Graph k cls colour -> k)	-- ^ fn to choose a node to potentially leave uncoloured if nothing is trivially colourable.
  -> Graph k cls colour		-- ^ the graph to scan
  -> ([k], [k], [(k, k)]) -- ^ triv colourable nodes, problem nodes, pairs of nodes to coalesce

colourScan iterative triv spill graph =
  colourScan_spin iterative triv spill graph [] [] []

colourScan_spin iterative triv spill graph
               ksTriv ksSpill kksCoalesce
  -- if the graph is empty then we're done
  | nullUM $ graphMap graph
  = (ksTriv, ksSpill, reverse kksCoalesce)

  -- Simplify:
  -- 
  -- Look for trivially colourable nodes.  If we can find some then
  -- remove them from the graph and go back for more.
  --
  | nsTrivFound@(_:_)
      <- scanGraph graph $ \node ->
           triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)
	     -- for iterative coalescing we only want non-move related
	     --	nodes here
	   && (not iterative || nullUS (nodeCoalesce node))
  , ksTrivFound <- map nodeId nsTrivFound
  , graph2 <- foldr (\k g -> let Just g' = delNode k g in g')
	            graph ksTrivFound
  = colourScan_spin iterative triv spill graph2
                   (ksTrivFound ++ ksTriv)
                   ksSpill
                   kksCoalesce

 -- Coalesce:
 -- 
 -- If we're doing iterative coalescing and no triv nodes are
 -- avaliable then it's time for a coalescing pass.
  | iterative
  = case coalesceGraph False triv graph of
      -- we were able to coalesce something; go back to Simplify and
      -- see if this frees up more nodes to be trivially colourable.
      (graph2, kksCoalesceFound @(_:_)) -> 
        colourScan_spin iterative triv spill graph2
		       ksTriv ksSpill (reverse kksCoalesceFound ++ kksCoalesce)

      -- Freeze:
      -- 
      -- nothing could be coalesced (or was triv), time to choose a
      -- node to freeze and give up on ever coalescing it.
      (graph2, []) ->
        case freezeOneInGraph graph2 of
	  -- we were able to freeze something hopefully this will free
	  -- up something for Simplify
	  (graph3, True) ->
	    colourScan_spin iterative triv spill graph3
			   ksTriv ksSpill kksCoalesce

	  -- we couldn't find something to freeze either
	  --	time for a spill
	  (graph3, False) ->
	    colourScan_spill iterative triv spill graph3
			    ksTriv ksSpill kksCoalesce

  -- spill time
  | otherwise
  = colourScan_spill iterative triv spill graph
                    ksTriv ksSpill kksCoalesce

-- Select:
-- 
-- We couldn't find any triv nodes or things to freeze or coalesce,
-- and the graph isn't empty yet.. We'll have to choose a spill
-- candidate and leave it uncoloured.
--
colourScan_spill iterative triv spill graph
                ksTriv ksSpill kksCoalesce =
  let kSpill      = spill graph
      Just graph' = delNode kSpill graph
  in colourScan_spin iterative triv spill graph'
                    ksTriv (kSpill : ksSpill) kksCoalesce

-- | Try to assign a colour to all these nodes.
assignColours :: 
     (Uniquable k, Uniquable cls, Uniquable colour,
      Eq colour, Pretty cls) =>
     UniqueMap cls (UniqueSet colour)
     -- ^ map of (node class -> set of colours available for this
     -- class).
  -> Graph k cls colour -- ^ the graph
  -> [k]               -- ^ nodes to assign a colour to.
  -> (Graph k cls colour, [k])
     -- ^ The coloured graph, and the nodes that didn't colour.
assignColours colours graph ks =
  assignColours' colours graph [] ks

 where
   assignColours' _ graph prob [] =
     (graph, prob)

   assignColours' colours graph prob (k:ks) =
     case assignColour colours k graph of
   	-- couldn't colour this node
    	Nothing     -> assignColours' colours graph (k : prob) ks
   	-- this node coloured ok, so do the rest
   	Just graph' -> assignColours' colours graph' prob ks

   assignColour colours u graph
     | Just c <- selectColour colours graph u
     = Just (setColour u c graph)

     | otherwise
     = Nothing
	
	
-- | Select a colour for a certain node taking into account
-- preferences, neighbors and exclusions.
--
-- Returns @Nothing@ if no colour can be assigned to this node.
--
selectColour :: 
     (Uniquable k, Uniquable cls, Uniquable colour,
      Eq colour, Pretty cls) =>
     UniqueMap cls (UniqueSet colour)
     -- ^ map of (node class -> set of colours available for this
     -- class).
  -> Graph k cls colour -- ^ the graph
  -> k                 -- ^ key of the node to select a colour for.
  -> Maybe colour

selectColour colours graph u =
  let
    Just node = lookupNode u graph

    -- lookup the available colours for the class of this node.
    colours_avail =
      case lookupUM (nodeClass node) colours of
        Nothing	-> error $ "selectColour: no colours available for class " ++ (pretty (nodeClass node))
        Just cs	-> cs

    -- Find colours we can't use because they're already being used by
    -- a node that conflicts with this one.
    Just nsConflicts =
       sequence $ map (`lookupNode` graph)
                $ toList $ nodeConflicts node
    	
    colours_conflict =
      fromListUS $ catMaybes $ map nodeColour nsConflicts
    
    -- the prefs of our neighbors
    colours_neighbor_prefs =
      fromListUS $ concatMap nodePreference nsConflicts

    -- colours that are still valid for us
    colours_ok_ex = colours_avail `differenceUS` nodeExclusions node
    colours_ok	= colours_ok_ex `differenceUS` colours_conflict
    			
    -- the colours that we prefer, and are still ok
    colours_ok_pref =
      fromListUS (nodePreference node) `intersectionUS` colours_ok

    -- the colours that we could choose while being nice to our neighbors
    colours_ok_nice = 
      colours_ok `differenceUS` colours_neighbor_prefs

    -- the best of all possible worlds..
    colours_ok_pref_nice =
      colours_ok_nice `intersectionUS` colours_ok_pref

    -- make the decision
    chooseColour
      -- everyone is happy, yay!
      | not $ nullUS colours_ok_pref_nice
      , c : _ <- filter (`memberUS` colours_ok_pref_nice)
    		        (nodePreference node)
      = Just c

      -- we've got one of our preferences
      | not $ nullUS colours_ok_pref	
      , c : _ <- filter (`memberUS` colours_ok_pref)
                        (nodePreference node)
      = Just c
    	
      -- it wasn't a preference, but it was still ok
      | not $ nullUS colours_ok
      , c : _ <- toList colours_ok
      = Just c
    	
      -- No colours were available for us this time.  Looks like we're
      -- going around the loop again..
      | otherwise
      = Nothing
   in
     chooseColour 

newtype Reg = R Int
  deriving (Eq, Ord, Show, Uniquable)

instance Pretty Reg where
  ppr (R n) = char 'r' <> int n

data Var = Var Int String | Reg Reg
  deriving (Eq, Ord, Show)

instance Pretty Var where
  ppr (Var _ n) = text n
  ppr (Reg r) = ppr r

instance Uniquable Var where
  getUnique (Var n _) = unsafeMkUnique (n * 2)
  getUnique (Reg (R n)) = unsafeMkUnique (n * 2 + 1)

data Class = Class
  deriving (Eq, Ord, Show)

instance Pretty Class where
  ppr _ = text "cls"

instance Uniquable Class where
  getUnique _ = unsafeMkUnique 1

test = pretty $ colourGraph True 0 classes triv spill gr
  where
    [a,b,c,d,e,f] = zipWith Var [1..6] (map (:[]) ['a'..])

    mk_node (k, confl, coal) =
      (newNode k Class){ nodeConflicts = fromListUS confl
                       , nodeCoalesce = fromListUS coal }

    classes :: UniqueMap Class (UniqueSet Reg)
    classes =
      fromListUM [(Class, regs)]

    regs = fromListUS [ R r | r <- [0..31] ]

    triv cls neighs cols =
      sizeUS neighs < sizeUS (regs `differenceUS` cols)

    spill _ = error "Cannot spill"

    nodes = map mk_node $
      [(a, [b,c,d], [])
      ,(b, [a,c,d], [])
      ,(c, [a,b], [])
      ,(d, [a,b], [])
      ,(e, [d], [])
      ,(f, [], [])]

    gr :: Graph Var Class Reg
    gr = foldr (\n g -> addNode (nodeId n) n g) newGraph nodes

