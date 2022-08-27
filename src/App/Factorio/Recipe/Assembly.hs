{-# LANGUAGE Safe #-}

module App.Factorio.Recipe.Assembly (get_recipes_assembly) where

import App.Facility as Facility (Facility, to_category)
import App.Recipe as Recipe (Recipe, create)
import App.Resource as Resource (Resource, from_name, to_name)
import App.Throughput as Throughput (Throughput (Throughput))

get_recipes_assembly :: Resource -> Facility -> [Recipe]
get_recipes_assembly res facility =
  case Facility.to_category facility of
    Just "assembly" -> case Resource.to_name res of
      "Copper Cable" ->
        [ Recipe.create
            facility
            0.5
            [Throughput (from_name "Copper Plate") 1.0 Nothing]
            [Throughput (from_name "Copper Cable") 2.0 Nothing]
        ]
      "Iron Stick" ->
        [ Recipe.create
            facility
            0.5
            [Throughput (from_name "Iron Plate") 1.0 Nothing]
            [Throughput (from_name "Iron Stick") 2.0 Nothing]
        ]
      "Iron Gear Wheel" ->
        [ Recipe.create
            facility
            0.5
            [Throughput (from_name "Iron Plate") 2.0 Nothing]
            [Throughput (from_name "Iron Gear Wheel") 1.0 Nothing]
        ]
      "Electronic Circuit" ->
        [ Recipe.create
            facility
            0.5
            [ Throughput (from_name "Iron Plate") 1.0 Nothing,
              Throughput (from_name "Copper Cable") 3.0 Nothing
            ]
            [Throughput (from_name "Electronic Circuit") 1.0 Nothing]
        ]
      "Advanced Circuit" ->
        [ Recipe.create
            facility
            6
            [ Throughput (from_name "Plastic Bar") 2.0 Nothing,
              Throughput (from_name "Copper Cable") 4.0 Nothing,
              Throughput (from_name "Electronic Circuit") 2.0 Nothing
            ]
            [Throughput (from_name "Advanced Circuit") 1.0 Nothing]
        ]
      "Processing Unit" ->
        [ Recipe.create
            facility
            10
            [ Throughput (from_name "Sulfuric Acid") 5.0 Nothing,
              Throughput (from_name "Advanced Circuit") 2.0 Nothing,
              Throughput (from_name "Electronic Circuit") 20.0 Nothing
            ]
            [Throughput (from_name "Processing Unit") 1.0 Nothing]
        ]
      "Engine Unit" ->
        [ Recipe.create
            facility
            10
            [ Throughput (from_name "Steel Plate") 1.0 Nothing,
              Throughput (from_name "Iron Gear Wheel") 1.0 Nothing,
              Throughput (from_name "Pipe") 2.0 Nothing
            ]
            [Throughput (from_name "Engine Unit") 1.0 Nothing]
        ]
      "Electric Engine Unit" ->
        [ Recipe.create
            facility
            10
            [ Throughput (from_name "Electronic Circuit") 2.0 Nothing,
              Throughput (from_name "Engine Unit") 1.0 Nothing,
              Throughput (from_name "Lubricant") 20.0 Nothing
            ]
            [Throughput (from_name "Electric Engine Unit") 1.0 Nothing]
        ]
      "Flying Robot Frame" ->
        [ Recipe.create
            facility
            20
            [ Throughput (from_name "Steel Plate") 1.0 Nothing,
              Throughput (from_name "Battery") 2.0 Nothing,
              Throughput (from_name "Electronic Circuit") 3.0 Nothing,
              Throughput (from_name "Electric Engine Unit") 1.0 Nothing
            ]
            [Throughput (from_name "Flying Robot Frame") 1.0 Nothing]
        ]
      _ -> []
    _ -> []
