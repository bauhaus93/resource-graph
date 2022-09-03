{-# LANGUAGE DeriveGeneric #-}


module App.ProductionGraph
  () where

import           App.Factory                    ( Factory )
import           App.Recipe                     ( ThroughputRate )
import           App.Resource                   ( Resource )
import           Prelude                        ( Double )



data ProductionGraph = Node

type DemandRate = Double


buildProductionGraph :: Factory -> ProductionGraph
buildProductionGraph factory = Node
