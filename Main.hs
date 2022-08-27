{-# LANGUAGE Safe #-}

import Data.Maybe (Maybe, maybe)
import Draw (draw)
import Factory (Factory, create_factory)
import ProductionChain (Node, get_node)
import Resource (from_name)
import Prelude (Float, IO, String, putStrLn, show, ($), (.), (>>=))

target_factory :: String
target_factory = "factorio"

main :: IO ()
main =
  putStrLn draw_graph

print_factory :: String
print_factory = maybe "ERROR" show $ Factory.create_factory $ target_factory

draw_graph :: String
draw_graph = (maybe "ERROR" draw maybe_production_chain)
  where
    maybe_production_chain =
      Factory.create_factory target_factory
        >>= get_production_chain "Processing Unit" 1000

get_production_chain :: String -> Float -> Factory -> Maybe Node
get_production_chain resource_name target_rate factory = (ProductionChain.get_node factory (Resource.from_name resource_name) target_rate)
