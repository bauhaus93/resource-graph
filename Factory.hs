module Factory (Factory, create_factory, Factory.to_string) where

import Facility
import Factorio
import Recipe
import Resource

data Factory = Factory
  { resources :: [Resource],
    facilites :: [Facility],
    recipes :: [Recipe]
  }

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
to_facility_string factory = join_strings (map Facility.to_string (to_facilities factory))

to_recipe_string :: Factory -> String
to_recipe_string factory = join_strings (map Recipe.to_string (to_recipes factory))

to_string :: Factory -> String
to_string factory =
  "+++ Resource Pool +++\n"
    ++ (to_resource_string factory)
    ++ "\n+++ Available Facilities +++\n"
    ++ (to_facility_string factory)
    ++ "\n+++ Available Recipes +++\n"
    ++ (to_recipe_string factory)

create_factory :: String -> Maybe Factory
create_factory name =
  case name of
    "factorio" -> Just (from_parts Factorio.get_resources Factorio.get_facilities Factorio.get_recipes)
    _ -> Nothing

from_parts :: [Resource] -> [Facility] -> ([Resource] -> [Facility] -> [Recipe]) -> Factory
from_parts resources facilites recipe_fn =
  Factory resources facilites (recipe_fn resources facilites)
