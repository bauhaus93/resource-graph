{-# LANGUAGE Safe #-}

module Graph (to_graph_node, GraphNode, to_node_name, to_children, to_node) where

import Data.List (zip, (++))
import ProductionChain (Node, to_inputs)
import Prelude (Integer, String, show, (+), (.))

data GraphNode = GraphNode
  { id :: Integer,
    node :: Node,
    children :: [GraphNode]
  }

to_graph_node :: Integer -> Node -> GraphNode
to_graph_node node_id current_node =
  GraphNode
    { id = node_id,
      node = current_node,
      children = (to_graph_nodes node_id . ProductionChain.to_inputs) current_node
    }

to_graph_nodes :: Integer -> [Node] -> [GraphNode]
to_graph_nodes parent_id nodes = [to_graph_node n_id n | (n_id, n) <- (zip child_ids nodes)]
  where
    child_ids = [i | i <- [parent_id + 1 ..]]

prefix_node_id :: Integer -> String
prefix_node_id = (++) "node_" . show

to_node_name :: GraphNode -> String
to_node_name = (prefix_node_id . to_id)

to_node :: GraphNode -> Node
to_node = node

to_id :: GraphNode -> Integer
to_id = id

to_children :: GraphNode -> [GraphNode]
to_children = children
