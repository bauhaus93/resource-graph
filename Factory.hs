{-# LANGUAGE Safe #-}

module Factory (Factory, create_factory, to_recipes) where

import Data.List (foldr, map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Facility (Facility)
import Factorio (get_facilities, get_recipes, get_resources)
import Recipe (Recipe)
import Resource (Resource, to_name)
import Prelude (Show, String, show)

data Factory = Factory
  { resources :: [Resource],
    facilites :: [Facility],
    recipes :: [Recipe]
  }

instance Show Factory where
  show factory =
    "+++ Resource Pool +++\n"
      ++ (to_resource_string factory)
      ++ "\n+++ Available Facilities +++\n"
      ++ (to_facility_string factory)
      ++ "\n+++ Available Recipes +++\n"
      ++ (to_recipe_string factory)

to_resources :: Factory -> [Resource.Resource]
to_resources factory = resources factory

to_facilities :: Factory -> [Facility.Facility]
to_facilities factory = facilites factory

to_recipes :: Factory -> [Recipe.Recipe]
to_recipes factory = recipes factory

join_strings :: [String] -> String
join_strings strs = foldr (\acc -> \el -> acc ++ "\n" ++ el) "" strs

to_resource_string :: Factory -> String
to_resource_string factory = join_strings (map Resource.to_name (to_resources factory))

to_facility_string :: Factory -> String
to_facility_string factory = join_strings (map show (to_facilities factory))

to_recipe_string :: Factory -> String
to_recipe_string factory = join_strings (map show (to_recipes factory))

create_factory :: String -> Maybe Factory
create_factory name =
  case name of
    "factorio" -> Just (from_parts Factorio.get_resources Factorio.get_facilities Factorio.get_recipes)
    _ -> Nothing

from_parts :: [Resource] -> [Facility] -> ([Resource] -> [Facility] -> [Recipe]) -> Factory
from_parts factory_resources factory_facilites recipe_fn =
  Factory factory_resources factory_facilites (recipe_fn factory_resources factory_facilites)
