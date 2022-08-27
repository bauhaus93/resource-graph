{-# LANGUAGE Safe #-}

import App.Draw as Draw (draw)
import App.Factory as Factory (Factory, create_factory)
import App.ProductionChain as ProductionChain (Node, get_node)
import App.Resource as Resource (from_name)
import Data.Maybe (Maybe, maybe)
import Prelude (Float, IO, String, putStrLn, show, ($), (>>=))

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
        >>= get_production_chain "Flying Robot Frame" 1000

get_production_chain :: String -> Float -> Factory -> Maybe Node
get_production_chain resource_name target_rate factory = (ProductionChain.get_node factory (Resource.from_name resource_name) target_rate)
