{-# LANGUAGE Safe #-}

module Facility (Facility, from_name, from_names, with_category, to_category, to_name, to_speed, create) where

import Data.List (map, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Prelude (Float, Show, String, show, (.))

data Facility = Facility
  { name :: String,
    speed :: Float,
    category :: Maybe String
  }

instance Show Facility where
  show facility = (to_name facility) ++ " (" ++ (show . to_speed) facility ++ ")"

create :: String -> String -> Float -> Facility
create facility_name facility_category facility_speed =
  Facility {name = facility_name, category = Just facility_category, speed = facility_speed}

from_name :: String -> Facility
from_name facility_name = Facility {name = facility_name, speed = 1.0, category = Nothing}

from_names :: [String] -> [Facility]
from_names names = map from_name names

with_category :: String -> Facility -> Facility
with_category category_name facility = facility {category = Just category_name}

to_name :: Facility -> String
to_name facility = name facility

to_category :: Facility -> Maybe String
to_category facility = category facility

to_speed :: Facility -> Float
to_speed facility = speed facility
