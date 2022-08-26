module Throughput (Throughput, multiply, create, to_quantity, with_probability, to_resource, calculate_demand_factor) where

import Resource

data Throughput = Throughput
  { resource :: Resource,
    quantity :: Float,
    probability :: Maybe Float
  }

instance Show Throughput where
  show throughput = (show . to_quantity) throughput ++ probability_string ++ " x '" ++ ((\e -> (++) e "'") . Resource.to_name . to_resource) throughput
    where
      probability_string =
        case to_maybe_probability throughput of
          Just probability -> " @ " ++ show (probability * 100) ++ "%"
          Nothing -> ""

create :: Resource -> Float -> Throughput
create res quantity = Throughput {resource = res, quantity = quantity, probability = Nothing}

with_probability :: Float -> Throughput -> Throughput
with_probability probability throughput = throughput {probability = Just probability}

multiply :: Float -> Throughput -> Throughput
multiply factor throughput = throughput {quantity = ((*) factor . to_quantity) throughput}

calculate_demand_factor :: Throughput -> Throughput -> Integer
calculate_demand_factor target base = round ((to_quantity target) / (to_quantity base))

to_quantity :: Throughput -> Float
to_quantity throughput = quantity throughput

to_maybe_probability :: Throughput -> Maybe Float
to_maybe_probability throughput = probability throughput

to_resource :: Throughput -> Resource
to_resource throughput = resource throughput
