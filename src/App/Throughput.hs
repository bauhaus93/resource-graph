{-# LANGUAGE DeriveGeneric #-}

module App.Throughput
  ( Throughput(Throughput)
  , Input
  , Output
  , Quantity
  , Probability
  , multiply
  ) where

import           App.Facility                   ( Facility
                                                , toSpeed
                                                )
import           App.Resource                   ( Resource(Resource) )
import           Data.List                      ( (++) )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( ($)
                                                , (*)
                                                , Double
                                                , Show
                                                , show
                                                )

data Throughput
  = Throughput Resource Quantity
  | ThroughputProbabilistic Resource Quantity Probability
  deriving (Generic)

instance FromJSON Throughput

type Input = Throughput

type Output = Throughput

type Quantity = Double

type Probability = Double

instance Show Throughput where
  show (Throughput (Resource name) quant) = show quant ++ " x " ++ name
  show (ThroughputProbabilistic res quant prob) =
    show (Throughput res quant) ++ " (" ++ show (100 * prob) ++ "%)"



multiply :: Double -> Throughput -> Throughput
multiply fac tp = case tp of
  Throughput res quant -> Throughput res (quant * fac)
  ThroughputProbabilistic res quant prob ->
    ThroughputProbabilistic res (quant * fac) prob
