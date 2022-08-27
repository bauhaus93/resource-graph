{-# LANGUAGE Safe #-}

module Draw (draw) where

import Data.List (foldr, map, (++))
import Facility (to_name)
import Graph (GraphNode, to_children, to_graph_node, to_node, to_node_name)
import ProductionChain (Node, to_recipe)
import Recipe (to_facility, to_name, to_output_resources)
import Resource (Resource, to_name)
import Prelude (Show, String, show, ($), (.))

newtype DrawableNode = DrawableNode GraphNode

instance Show DrawableNode where
  show (DrawableNode n) = foldr (++) "" [draw_node_str n, draw_edge_str n, draw_children_str n]

draw :: Node -> String
draw = (wrap_digraph . show . (DrawableNode . (to_graph_node 0)))

wrap_digraph :: String -> String
wrap_digraph content =
  foldr (++) "" [draw_graph_header, content, draw_graph_footer]

draw_graph_header :: String
draw_graph_header = "digraph resource_graph {\nnode [shape=record];\nfontname=Courier;\n"

draw_graph_footer :: String
draw_graph_footer = "\n}"

draw_node_str :: GraphNode -> String
draw_node_str graph_node =
  foldr
    (++)
    ""
    [node_name, "[label=\"", label, "\" fillcolor=", color, "; style=filled]\n"]
  where
    node_name = to_node_name graph_node
    label = to_node_label graph_node
    color = "aliceblue"

to_node_label :: GraphNode -> String
to_node_label graph_node = "{" ++ title ++ "|{" ++ outputs ++ "}}"
  where
    title = Facility.to_name . Recipe.to_facility . ProductionChain.to_recipe . Graph.to_node $ graph_node
    outputs = foldr (++) "" $ to_output_resource_names graph_node

to_output_resource_names :: GraphNode -> [String]
to_output_resource_names graph_node = map Resource.to_name $ (Recipe.to_output_resources . ProductionChain.to_recipe . Graph.to_node) graph_node

to_edge_label :: GraphNode -> GraphNode -> String
to_edge_label parent_node child_node = "[label=\"EDGE\"]"

draw_edge_str :: GraphNode -> String
draw_edge_str graph_node = (foldr (++) "" . map (to_edge_str graph_node)) $ Graph.to_children graph_node
  where
    to_edge_str :: GraphNode -> GraphNode -> String
    to_edge_str parent child =
      foldr (++) "" $
        [ Graph.to_node_name child,
          " -> ",
          Graph.to_node_name parent,
          to_edge_label parent child,
          "\n"
        ]

draw_children_str :: GraphNode -> String
draw_children_str graph_node = foldr (++) "" (map (show . DrawableNode) (Graph.to_children graph_node))
