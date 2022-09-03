{-# LANGUAGE DeriveGeneric #-}

module App.Facility (Facility (Facility), FacilityType) where

import Data.List (map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Prelude (Float, Show, String, show, (.))
import GHC.Generics(Generic)
import Data.Yaml (FromJSON)

data Facility = Facility
  { name :: String,
    speed :: Float,
    category :: Maybe String
  } deriving Generic

instance FromJSON Facility


type FacilityType = String

