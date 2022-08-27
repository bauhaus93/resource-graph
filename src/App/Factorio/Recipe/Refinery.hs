{-# LANGUAGE Safe #-}

module App.Factorio.Recipe.Refinery (get_recipes_refinery) where

import App.Facility as Facility (Facility, to_category)
import App.Recipe as Recipe (Recipe, create)
import App.Resource as Resource (Resource, from_name, to_name)
import App.Throughput as Throughput (Throughput (Throughput))
import Data.List (elem)
import Data.Maybe (Maybe (Just, Nothing))
import Prelude ()

get_recipes_refinery :: Resource -> Facility -> [Recipe]
get_recipes_refinery res facility =
  case Facility.to_category facility of
    Just "refinery" -> case Resource.to_name res of
      "Petroleum Gas" ->
        [ Recipe.create
            facility
            5
            [Throughput (from_name "Crude Oil") 100 Nothing]
            [Throughput (from_name "Petroleum Gas") 45 Nothing]
        ]
      name
        | name `elem` ["Heavy Oil", "Light Oil", "Petroleum Gas"] ->
          [ Recipe.create
              facility
              5
              [ Throughput (from_name "Crude Oil") 100 Nothing,
                Throughput (from_name "Water") 50 Nothing
              ]
              [ Throughput (from_name "Heavy Oil") 25 Nothing,
                Throughput (from_name "Light Oil") 45 Nothing,
                Throughput (from_name "Petroleum Gas") 55 Nothing
              ],
            Recipe.create
              facility
              5
              [ Throughput (from_name "Coal") 10 Nothing,
                Throughput (from_name "Heavy Oil") 25 Nothing,
                Throughput (from_name "Steam") 50 Nothing
              ]
              [ Throughput (from_name "Heavy Oil") 90 Nothing,
                Throughput (from_name "Light Oil") 20 Nothing,
                Throughput (from_name "Petroleum Gas") 10 Nothing
              ]
          ]
      _ -> []
    _ -> []
