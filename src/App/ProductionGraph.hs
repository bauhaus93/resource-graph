{-# LANGUAGE DeriveGeneric #-}

module App.ProductionGraph
  () where

import           App.Factory                    ( Factory )
import           App.Recipe                     ( ThroughputRate )
import           App.Resource                   ( Resource )
import           Prelude                        ( Double )



data ProductionGraph = Node
  { throughput_rate :: ThroughputRate
  , children        :: [ProductionGraph]
  }


{-- TODO: buildProductionGraph :: Factory -> ProductionGraph
buildProductionGraph factory = ProductionGraph  --}
