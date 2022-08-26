import Factory
import ProductionChain
import Resource
import Throughput

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
draw_graph root = show root
