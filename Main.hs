{-# LANGUAGE Safe #-}

import Data.List (foldr, (++))
import Data.Maybe (Maybe, maybe)
import Factory (Factory, create_factory)
import ProductionChain (Node, get_node, to_graph_output)
import Resource (from_name)
import Prelude (Float, IO, String, putStrLn, (.), (>>=))

target_factory :: String
target_factory = "factorio"

main :: IO ()
main =
  putStrLn
    (maybe "ERROR" draw_graph maybe_production_chain)
  where
    maybe_production_chain =
      Factory.create_factory target_factory
        >>= get_production_chain "Copper Cable" 1000

get_production_chain :: String -> Float -> Factory -> Maybe Node
get_production_chain resource_name target_rate factory = (ProductionChain.get_node factory (Resource.from_name resource_name) target_rate)

draw_graph :: Node -> String
draw_graph root = (make_digraph . ProductionChain.to_graph_output "root") root

make_digraph :: String -> String
make_digraph content =
  foldr (++) "" [draw_graph_header, content, draw_graph_footer]

draw_graph_header :: String
draw_graph_header = "digraph resource_graph {\nnode [shape=record];\nfontname=Courier;\n"

draw_graph_footer :: String
draw_graph_footer = "\n}"
