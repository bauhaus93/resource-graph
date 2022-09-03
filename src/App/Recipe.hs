{-# LANGUAGE DeriveGeneric #-}

module App.Recipe
  ( Recipe,
  )
where

import App.Facility (FacilityType, Facility, to_speed)
import App.Throughput as Throughput (Input, Output, multiply, ThroughputRate(ThroughputRate))
import Data.List (intercalate, map, (++))
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Prelude (Double, Eq, Integer, Show, show, ($), String, (.), (/))

data Recipe = Recipe
  { name :: String,
    facility_type :: FacilityType,
    production_time :: Double,
    input :: [Input],
    output :: [Output]
  }
  deriving (Generic)

instance FromJSON Recipe

instance Show Recipe where
  show rec =
    name rec
      ++ " ("
      ++ (show . production_time) rec
      ++ "s): "
      ++ input_str
      ++ " ==> "
      ++ output_str
    where
      input_str = intercalate ", " $ map show $ input rec
      output_str = intercalate ", " $ map show $ output rec

calculate_rates::Recipe -> Facility -> ThroughputRate
calculate_rates rec fac = ThroughputRate input_rates output_rates
  where
    input_rates = map (multiply rate) $ input rec
    output_rates = map (multiply rate) $ output rec
    rate = 1 / (production_time rec / to_speed fac)
