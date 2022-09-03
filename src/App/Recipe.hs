{-# LANGUAGE DeriveGeneric #-}

module App.Recipe (Recipe) where

import Data.List (foldr, head, map, tail, (++))
import Data.Maybe (Maybe (Just, Nothing))
import App.Facility as Facility (Facility, FacilityType)
import App.Resource as Resource (Resource)
import App.Throughput as Throughput (Input, Output, Throughput (Throughput))
import Prelude (Eq, Float, Integer, Ord, Show, String, abs, ceiling, show, ($), (-), (.), (/), (<$>), (<=), (==), (>>=))
import GHC.Generics(Generic)
import Data.Yaml (FromJSON)

data Recipe = Recipe
  { name :: Maybe String,
    facility_type :: FacilityType, 
    production_time :: Float,
    input :: [Input],
    output :: [Output]
  } deriving Generic


instance FromJSON Recipe
