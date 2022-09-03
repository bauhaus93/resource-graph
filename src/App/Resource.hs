{-# LANGUAGE DeriveGeneric #-}

module App.Resource (Resource(Resource)) where

import Data.List (map)
import Prelude (Eq, Show, String, show, (==))
import GHC.Generics(Generic)
import Data.Yaml (FromJSON)

data Resource = Resource String
   deriving Generic

instance FromJSON Resource

