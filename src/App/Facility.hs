{-# LANGUAGE DeriveGeneric #-}

module App.Facility
  ( Facility (Facility),
    FacilityType,
    to_speed
  )
where

import Data.List (map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Prelude (Double, Show, String, show, (.))

data Facility = Facility
  { name :: String,
    speed :: Double,
    facility_type :: String
  }
  deriving (Generic)

instance FromJSON Facility

type FacilityType = String

instance Show Facility where
  show fac =
    name fac ++ " (x" ++ (show . speed) fac ++ ", " ++ facility_type fac ++ ")"


to_speed::Facility -> Double
to_speed = speed
