{-# LANGUAGE Safe #-}

module App.Resource (Resource, create_resources, to_name, from_name) where

import Data.List (map)
import Prelude (Eq, Show, String, show, (==))

data Resource = Resource
  { name :: String
  }

instance Eq Resource where
  (==) l r = (to_name l) == (to_name r)

instance Show Resource where
  show = to_name

create_resources :: [String] -> [Resource]
create_resources = map from_name

to_name :: Resource -> String
to_name res = name res

from_name :: String -> Resource
from_name res_name = Resource {name = res_name}
