{-# LANGUAGE Safe #-}

module App.Factorio.Recipe.Furnance (get_recipes_furnance) where

import App.Facility as Facility (Facility, to_category)
import App.Recipe as Recipe (Recipe, create)
import App.Resource as Resource (Resource, from_name, to_name)
import App.Throughput as Throughput (Throughput (Throughput))

get_recipes_furnance :: Resource -> Facility -> [Recipe]
get_recipes_furnance res facility =
  case Facility.to_category facility of
    Just "furnance" -> case Resource.to_name res of
      "Iron Plate" ->
        [ Recipe.create
            facility
            3.2
            [Throughput (from_name "Iron Ore") 1.0 Nothing]
            [Throughput (from_name "Iron Plate") 1.0 Nothing]
        ]
      "Copper Plate" ->
        [ Recipe.create
            facility
            3.2
            [Throughput (from_name "Copper Ore") 1.0 Nothing]
            [Throughput (from_name "Copper Plate") 1.0 Nothing]
        ]
      "Steel Plate" ->
        [ Recipe.create
            facility
            16
            [Throughput (from_name "Iron Plate") 5.0 Nothing]
            [Throughput (from_name "Steel Plate") 1.0 Nothing]
        ]
      _ -> []
    _ -> []
