{-# LANGUAGE DeriveGeneric #-}

module App.Resource
  ( Resource (Resource),
  )
where

import Data.List (map)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Prelude (Eq, Show, String, show, (==))

data Resource
  = Resource String
  deriving (Generic)

instance FromJSON Resource
