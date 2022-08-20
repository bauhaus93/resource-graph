module Factorio (get_resources, get_facilities, Factorio.get_recipes) where

import Facility
import Recipe
import Resource
import Throughput

ore_names :: [String]
ore_names = ["Iron Ore", "Copper Ore", "Stone Ore", "Coal", "Uranium"]

intermediate_names :: [String]
intermediate_names = ["Iron Plate", "Copper Plate", "Copper Cable"]

resource_names :: [String]
resource_names = foldr (++) [] [ore_names, intermediate_names]

get_specific_recipe :: Resource -> Facility -> [Recipe]
get_specific_recipe res facility = case (res_name, facility_name, facility_category) of
  ("Iron Plate", _, Just "furnance") ->
    [ Recipe.create
        facility
        3.2
        [Throughput.create (Resource.from_name "Iron Ore") 1.0]
        [Throughput.create (Resource.from_name "Iron Plate") 1.0]
    ]
  ("Copper Plate", _, Just "furnance") ->
    [ Recipe.create
        facility
        3.2
        [Throughput.create (Resource.from_name "Copper Ore") 1.0]
        [Throughput.create (Resource.from_name "Copper Plate") 1.0]
    ]
  ("Copper Cable", _, Just "assembly") ->
    [ Recipe.create
        facility
        0.5
        [Throughput.create (Resource.from_name "Copper Plate") 1.0]
        [Throughput.create (Resource.from_name "Copper Cable") 2.0]
    ]
  (_, _, _) -> []
  where
    res_name = Resource.to_name res
    facility_name = Facility.to_name facility
    facility_category = Facility.to_category facility

get_resources :: [Resource]
get_resources = map Resource.from_name resource_names

get_facilities :: [Facility]
get_facilities = furnances ++ assemblies
  where
    furnances =
      [ Facility.create "Stone Furnance" "furnance" 1.0,
        Facility.create "Steel Furnance" "furnance" 2.0,
        Facility.create "Electric Furnance" "furnance" 2.0
      ]
    assemblies =
      [ Facility.create "Assembly MK 1" "assembly" 0.5,
        Facility.create "Assembly MK 2" "assembly" 0.75,
        Facility.create "Assembly MK 3" "assembly" 1.25
      ]

get_recipes :: [Resource] -> [Facility] -> [Recipe]
get_recipes resources facilities = res_fac_prod >>= \(res, fac) -> (get_specific_recipe res fac)
  where
    res_fac_prod = [(res, fac) | res <- resources, fac <- facilities]
