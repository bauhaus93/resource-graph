{-# LANGUAGE Safe #-}

module App.Graph (to_graph_node, GraphNode, to_children, to_node, to_id) where

import App.ProductionChain as ProductionChain (Node, to_inputs)
import Data.List (zip, (++))
import Prelude (Integer, String, show, (.))

data GraphNode = GraphNode
  { id :: String,
    node :: Node,
    children :: [GraphNode]
  }

to_graph_node :: String -> Node -> GraphNode
to_graph_node node_id current_node =
  GraphNode
    { id = node_id,
      node = current_node,
      children = (to_graph_nodes node_id . ProductionChain.to_inputs) current_node
    }

to_graph_nodes :: String -> [Node] -> [GraphNode]
to_graph_nodes parent_id nodes = [to_graph_node n_id n | (n_id, n) <- (zip child_ids nodes)]
  where
    child_ids = [parent_id ++ "_" ++ (show i) | i <- [0 :: Integer ..]]

to_node :: GraphNode -> Node
to_node = node

to_id :: GraphNode -> String
to_id = id

to_children :: GraphNode -> [GraphNode]
to_children = children
