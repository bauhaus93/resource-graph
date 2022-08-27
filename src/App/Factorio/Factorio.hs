{-# LANGUAGE Safe #-}

module App.Factorio.Factorio (get_resources, get_facilities, get_recipes) where

import App.Facility as Facility (Facility, to_category, to_name)
import App.Factorio.Facilities (get_facilities)
import App.Factorio.Names (get_resource_names)
import App.Factorio.Recipe.Recipe (get_recipes_assembly, get_recipes_chemical, get_recipes_furnance)
import App.Recipe as Recipe (Recipe)
import App.Resource as Resource (Resource, from_name, to_name)
import Data.List (map)
import Data.Maybe (Maybe (Just))
import Prelude ((>>=))

get_specific_recipe :: Resource -> Facility -> [Recipe]
get_specific_recipe res facility = case (res_name, facility_name, facility_category) of
  (_, _, Just "chemical") -> get_recipes_chemical res facility
  (_, _, Just "furnance") -> get_recipes_furnance res facility
  (_, _, Just "assembly") -> get_recipes_assembly res facility
  (_, _, _) -> []
  where
    res_name = Resource.to_name res
    facility_name = Facility.to_name facility
    facility_category = to_category facility

get_resources :: [Resource]
get_resources = map from_name get_resource_names

get_recipes :: [Resource] -> [Facility] -> [Recipe]
get_recipes resources facilities = res_fac_prod >>= \(res, fac) -> (get_specific_recipe res fac)
  where
    res_fac_prod = [(res, fac) | res <- resources, fac <- facilities]
