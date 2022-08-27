{-# LANGUAGE Safe #-}

module App.Factorio.Recipe.Chemical (get_recipes_chemical) where

import App.Facility as Facility (Facility, to_category)
import App.Recipe as Recipe (Recipe, create)
import App.Resource as Resource (Resource, from_name, to_name)
import App.Throughput as Throughput (Throughput (Throughput))

get_recipes_chemical :: Resource -> Facility -> [Recipe]
get_recipes_chemical res facility =
  case Facility.to_category facility of
    Just "chemical" -> case Resource.to_name res of
      "Solid Fuel" ->
        [ Recipe.create
            facility
            2
            [Throughput (from_name "Light Oil") 10.0 Nothing]
            [Throughput (from_name "Solid Fuel") 1.0 Nothing],
          Recipe.create
            facility
            2
            [Throughput (from_name "Heavy Oil") 20.0 Nothing]
            [Throughput (from_name "Solid Fuel") 1.0 Nothing],
          Recipe.create
            facility
            2
            [Throughput (from_name "Petroleum Gas") 20.0 Nothing]
            [Throughput (from_name "Solid Fuel") 1.0 Nothing]
        ]
      "Plastic Bar" ->
        [ Recipe.create
            facility
            1
            [ Throughput (from_name "Coal") 1.0 Nothing,
              Throughput (from_name "Petroleum Gas") 20.0 Nothing
            ]
            [Throughput (from_name "Plastic Bar") 2.0 Nothing]
        ]
      "Sulfur" ->
        [ Recipe.create
            facility
            1
            [ Throughput (from_name "Water") 20.0 Nothing,
              Throughput (from_name "Petroleum Gas") 30.0 Nothing
            ]
            [Throughput (from_name "Sulfur") 2.0 Nothing]
        ]
      "Battery" ->
        [ Recipe.create
            facility
            4
            [ Throughput (from_name "Copper Plate") 1.0 Nothing,
              Throughput (from_name "Iron Plate") 1.0 Nothing,
              Throughput (from_name "Sulfuric Acid") 20.0 Nothing
            ]
            [Throughput (from_name "Battery") 1.0 Nothing]
        ]
      "Explosives" ->
        [ Recipe.create
            facility
            4
            [ Throughput (from_name "Coal") 1.0 Nothing,
              Throughput (from_name "Sulfur") 1.0 Nothing,
              Throughput (from_name "Water") 10.0 Nothing
            ]
            [Throughput (from_name "Explosives") 2.0 Nothing]
        ]
      "Uranium-235" ->
        {- or 238 -}
        [ Recipe.create
            facility
            12
            [ Throughput (from_name "Uranium Ore") 10.0 Nothing
            ]
            [ Throughput (from_name "Uranium-235") 1.0 (Just 0.993),
              Throughput (from_name "Uranium-238") 1.0 (Just 0.007)
            ]
        ]
      _ -> []
    _ -> []
