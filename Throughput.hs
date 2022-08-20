module Throughput (Throughput, to_string, create, to_quantity) where

import Resource

data Throughput = Throughput
  { resource :: Resource,
    quantity :: Float
  }

create :: Resource -> Float -> Throughput
create res quantity = Throughput {resource = res, quantity = quantity}

to_string :: Throughput -> String
to_string throughput = (show . to_quantity) throughput ++ " x '" ++ ((\e -> (++) e "'") . Resource.to_name . to_resource) throughput

to_quantity :: Throughput -> Float
to_quantity throughput = quantity throughput

to_resource :: Throughput -> Resource
to_resource throughput = resource throughput
