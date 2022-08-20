module Facility (Facility, from_name, from_names, with_category, to_category, to_name, to_speed, to_string, create) where

data Facility = Facility
  { name :: String,
    speed :: Float,
    category :: Maybe String
  }

create :: String -> String -> Float -> Facility
create name category speed = Facility {name = name, category = Just category, speed = speed}

from_name :: String -> Facility
from_name name = Facility {name = name, speed = 1.0, category = Nothing}

from_names :: [String] -> [Facility]
from_names names = map from_name names

with_category :: String -> Facility -> Facility
with_category category_name facility = facility {category = Just category_name}

with_speed :: Float -> Facility -> Facility
with_speed speed facility = facility {speed = speed}

to_string :: Facility -> String
to_string facility = (to_name facility) ++ " (" ++ (show . to_speed) facility ++ ")"

to_name :: Facility -> String
to_name facility = name facility

to_category :: Facility -> Maybe String
to_category facility = category facility

to_speed :: Facility -> Float
to_speed facility = speed facility
