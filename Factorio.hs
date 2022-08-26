module Factorio (get_resources, get_facilities, Factorio.get_recipes) where

import Facility
import Recipe
import Resource
import Throughput

ore_names :: [String]
ore_names = ["Wood", "Iron Ore", "Copper Ore", "Stone Ore", "Coal", "Uranium Ore"]

intermediate_names :: [String]
intermediate_names =
  [ "Iron Plate",
    "Copper Plate",
    "Solid Fuel",
    "Steel Plate",
    "Plastic Bar",
    "Sulfur",
    "Battery",
    "Explosives",
    "Copper Cable",
    "Iron Stick",
    "Iron Gear Wheel",
    "Electronic Circuit",
    "Advanced Circuit",
    "Processing Unit",
    "Engine Unit",
    "Electric Engine Unit",
    "Flying Robot Frame",
    "Rocket Part",
    "Rocket Control Unit",
    "Low Density Structure",
    "Rocket Fuel",
    "Nuclear Fuel",
    "Uranium-235",
    "Uranium-238",
    "Uranium Fuel Cell",
    "Used Up Uranium Fuel Cell"
  ]

fluid_names :: [String]
fluid_names = ["Crude Oil", "Heavy Oil", "Light Oil", "Lubricant", "Petroleum Gas", "Sulfuric Acid", "Water", "Steam"]

resource_names :: [String]
resource_names = foldr (++) [] [ore_names, fluid_names, intermediate_names]

get_specific_recipe :: Resource -> Facility -> [Recipe]
get_specific_recipe res facility = case (res_name, facility_name, facility_category) of
  (_, _, Just "chemical") -> get_recipes_chemical res facility
  (_, _, Just "furnance") -> get_recipes_furnance res facility
  (_, _, Just "assembly") -> get_recipes_assembly res facility
  (_, _, _) -> []
  where
    res_name = Resource.to_name res
    facility_name = Facility.to_name facility
    facility_category = Facility.to_category facility

get_recipes_assembly :: Resource -> Facility -> [Recipe]
get_recipes_assembly res facility =
  case Facility.to_category facility of
    Just "assembly" -> case Resource.to_name res of
      "Copper Cable" ->
        [ Recipe.create
            facility
            0.5
            [Throughput.create (Resource.from_name "Copper Plate") 1.0]
            [Throughput.create (Resource.from_name "Copper Cable") 2.0]
        ]
      _ -> []

get_recipes_furnance :: Resource -> Facility -> [Recipe]
get_recipes_furnance res facility =
  case Facility.to_category facility of
    Just "furnance" -> case Resource.to_name res of
      "Iron Plate" ->
        [ Recipe.create
            facility
            3.2
            [Throughput.create (Resource.from_name "Iron Ore") 1.0]
            [Throughput.create (Resource.from_name "Iron Plate") 1.0]
        ]
      "Copper Plate" ->
        [ Recipe.create
            facility
            3.2
            [Throughput.create (Resource.from_name "Copper Ore") 1.0]
            [Throughput.create (Resource.from_name "Copper Plate") 1.0]
        ]
      "Steel Plate" ->
        [ Recipe.create
            facility
            16
            [Throughput.create (Resource.from_name "Iron Plate") 5.0]
            [Throughput.create (Resource.from_name "Steel Plate") 1.0]
        ]
      _ -> []

get_recipes_chemical :: Resource -> Facility -> [Recipe]
get_recipes_chemical res facility =
  case Facility.to_category facility of
    Just "chemical" -> case Resource.to_name res of
      "Solid Fuel" ->
        [ Recipe.create
            facility
            2
            [Throughput.create (Resource.from_name "Light Oil") 10.0]
            [Throughput.create (Resource.from_name "Solid Fuel") 1.0],
          Recipe.create
            facility
            2
            [Throughput.create (Resource.from_name "Heavy Oil") 20.0]
            [Throughput.create (Resource.from_name "Solid Fuel") 1.0],
          Recipe.create
            facility
            2
            [Throughput.create (Resource.from_name "Petroleum Gas") 20.0]
            [Throughput.create (Resource.from_name "Solid Fuel") 1.0]
        ]
      "Plastic Bar" ->
        [ Recipe.create
            facility
            1
            [ Throughput.create (Resource.from_name "Coal") 1.0,
              Throughput.create (Resource.from_name "Petroleum Gas") 20.0
            ]
            [Throughput.create (Resource.from_name "Plastic Bar") 2.0]
        ]
      "Sulfur" ->
        [ Recipe.create
            facility
            1
            [ Throughput.create (Resource.from_name "Water") 20.0,
              Throughput.create (Resource.from_name "Petroleum Gas") 30.0
            ]
            [Throughput.create (Resource.from_name "Sulfur") 2.0]
        ]
      "Battery" ->
        [ Recipe.create
            facility
            4
            [ Throughput.create (Resource.from_name "Copper Plate") 1.0,
              Throughput.create (Resource.from_name "Iron Plate") 1.0,
              Throughput.create (Resource.from_name "Sulfuric Acid") 20.0
            ]
            [Throughput.create (Resource.from_name "Battery") 1.0]
        ]
      "Explosives" ->
        [ Recipe.create
            facility
            4
            [ Throughput.create (Resource.from_name "Coal") 1.0,
              Throughput.create (Resource.from_name "Sulfur") 1.0,
              Throughput.create (Resource.from_name "Water") 10.0
            ]
            [Throughput.create (Resource.from_name "Explosives") 2.0]
        ]
      "Uranium-235" ->
        {- or 238 -}
        [ Recipe.create
            facility
            12
            [ Throughput.create (Resource.from_name "Uranium Ore") 10.0
            ]
            [ (Throughput.with_probability 0.993 . Throughput.create (Resource.from_name "Uranium-235")) 1.0,
              (Throughput.with_probability 0.007 . Throughput.create (Resource.from_name "Uranium-238")) 1.0
            ]
        ]
      _ -> []

get_resources :: [Resource]
get_resources = map Resource.from_name resource_names

get_facilities :: [Facility]
get_facilities =
  [ Facility.create "Stone Furnance" "furnance" 1.0,
    Facility.create "Steel Furnance" "furnance" 2.0,
    Facility.create "Electric Furnance" "furnance" 2.0,
    Facility.create "Assembly MK 1" "assembly" 0.5,
    Facility.create "Assembly MK 2" "assembly" 0.75,
    Facility.create "Assembly MK 3" "assembly" 1.25,
    Facility.create "Oil Refinery" "refinery" 1.0,
    Facility.create "Chemical Plant" "chemical" 1.0,
    Facility.create "Centrifuge" "centrifuge" 1.0
  ]

get_recipes :: [Resource] -> [Facility] -> [Recipe]
get_recipes resources facilities = res_fac_prod >>= \(res, fac) -> (get_specific_recipe res fac)
  where
    res_fac_prod = [(res, fac) | res <- resources, fac <- facilities]
