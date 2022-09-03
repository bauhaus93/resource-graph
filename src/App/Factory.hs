{-# LANGUAGE DeriveGeneric #-}

module App.Factory (Factory(Factory)) where

import Data.List (foldr, map, (++))
import GHC.Generics(Generic)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Yaml (FromJSON)
import App.Facility as Facility (Facility)
import App.Recipe as Recipe (Recipe)
import App.Resource as Resource (Resource)
import Prelude (Show, String, show, (.), (<$>), (<*>))

data Factory = Factory
  { resources :: [Resource],
    facilites :: [Facility],
    recipes :: [Recipe]
  } deriving Generic

instance FromJSON Factory

instance Show Factory where
  show f = "Factory"
