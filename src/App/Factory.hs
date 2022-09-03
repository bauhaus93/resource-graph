{-# LANGUAGE DeriveGeneric #-}

module App.Factory (Factory(Factory)) where

import Data.List (foldr, map, (++))
import GHC.Generics(Generic)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Yaml (FromJSON)
import App.Facility as Facility (Facility)
import App.Recipe as Recipe (Recipe)
import App.Resource as Resource (Resource)
import Prelude (Show, String, show, (.), ($), (<$>), (<*>), concatMap)

data Factory = Factory
  { facilites :: [Facility],
    recipes :: [Recipe]
  } deriving Generic

instance FromJSON Factory

instance Show Factory where
  show fac = facility_string ++ recipe_string
    where
      show_list::Show a => String->[a]->String
      show_list title elements =  title ++ "\n" ++ concatMap (\e -> "\t" ++ show e ++ "\n") elements
      facility_string = show_list "*** Facilities ***"  $ facilites fac
      recipe_string = show_list "*** Recipes ***"  $ recipes fac
