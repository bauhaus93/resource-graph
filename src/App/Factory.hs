{-# LANGUAGE DeriveGeneric #-}

module App.Factory (Factory, create_factory, to_recipes) where

import Data.List (foldr, map, (++))
import GHC.Generics(Generic)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Yaml (FromJSON)
import App.Facility as Facility (Facility)
import App.Recipe as Recipe (Recipe)
import App.Resource as Resource (Resource, to_name)
import Prelude (Show, String, show, (.), (<$>), (<*>))

data Factory = Factory
  { resources :: [Resource],
    facilites :: [Facility],
    recipes :: [Recipe]
  } deriving Generic

instance FromJSON Factory

instance Show Factory where
  show factory =
    "+++ Resource Pool +++\n"
      ++ (to_resource_string factory)
      ++ "\n+++ Available Facilities +++\n"
      ++ (to_facility_string factory)
      ++ "\n+++ Available Recipes +++\n"
      ++ (to_recipe_string factory)

to_resources :: Factory -> [Resource]
to_resources factory = resources factory

to_facilities :: Factory -> [Facility]
to_facilities factory = facilites factory

to_recipes :: Factory -> [Recipe]
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
    _ -> Nothing

from_parts :: [Resource] -> [Facility] -> ([Resource] -> [Facility] -> [Recipe]) -> Factory
from_parts factory_resources factory_facilites recipe_fn =
  Factory factory_resources factory_facilites (recipe_fn factory_resources factory_facilites)
