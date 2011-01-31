-- Source code from GHC  (BSD3)
-- Authors:  (c) Ben Lippmeier, Ian Lynagh, Simon Marlow

module Lambdachine.Utils.Graph.Base where

import Lambdachine.Utils.Unique
import Lambdachine.Utils.Pretty
import Data.Monoid
import Data.Maybe ( fromMaybe )

-- | A function to check if a node is trivially colourable.
--
-- For graphs who's colour classes are disjoint then a node is
-- /trivially colourable/ when it has less neighbors and exclusions
-- than available colours for that node.
--
-- For graphs whose colour classes overlap, i.e., some colours alias
-- other colours, then this can be a bit more tricky. There is a
-- general way to calculate this, but it's likely be too slow for use
-- in the code. The colouring algorithm takes a canned function which
-- can be optimised by the user to be specific to the specific graph
-- being coloured.
--
-- For details, see \"A Generalised Algorithm for Graph-Colouring
-- Register Allocation\" Smith, Ramsey, Holloway - PLDI 2004.
--
-- The arguments to the function are:
--
--  * the class of the node we're trying to colour.
--
--  * the node's neighbors.
--
--  * the node's exclusions.
--
type Triv k cls colour =
  cls -> UniqueSet k -> UniqueSet colour -> Bool

-- | The Interference graph.
data Graph k cls colour = Graph {
  -- | All active nodes in the graph.
  graphMap :: UniqueMap k (Node k cls colour) }
 deriving Show

instance (Pretty k, Pretty cls, Pretty colour)
  => Pretty (Graph k cls colour) where
  ppr Graph{graphMap = mp} =
    braces $ sep (map pp1 (elementsUM mp))
   where pp1 n =
           char '[' <> ppr (nodeId n) <> colon <>
           maybe (char '?') ppr (nodeColour n) <> comma <+>
           ppr (nodeConflicts n) <> comma <+>
           ppr (nodeCoalesce n) <> char ']'

-- | An empty graph.
newGraph :: Graph k cls colour
newGraph = Graph { graphMap = mempty }

modifyGraphMap ::
     Graph k cls colour
  -> (UniqueMap k (Node k cls colour)
        -> UniqueMap k (Node k cls colour))
  -> Graph k cls colour
modifyGraphMap graph f =
  graph { graphMap = f (graphMap graph) }



data Node k cls colour = Node {
  -- | A unique identifier for this node.
  nodeId :: k,
  -- | The class of this node; determines the set of colours that
  -- can be used.
  nodeClass :: cls,
  -- | The colour of this node, if any.
  nodeColour :: Maybe colour,
  -- | Neighbors which must be coloured differently to this node.
  nodeConflicts :: UniqueSet k,
  -- | Colours that cannot be used by this node.
  nodeExclusions :: UniqueSet colour,
  -- | Colours that this node would prefer to be, in descending
  -- order.
  nodePreference :: [colour],
  -- | Neighbors that this node would like to be coloured the same as.
  nodeCoalesce :: UniqueSet k }
 deriving Show

newNode :: k -> cls -> Node k cls colour
newNode k cls =
  Node { nodeId = k,
         nodeClass = cls,
         nodeColour = Nothing,
         nodeConflicts = mempty,
         nodeExclusions = mempty,
         nodePreference = [],
         nodeCoalesce = mempty }