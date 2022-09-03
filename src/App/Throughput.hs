{-# LANGUAGE DeriveGeneric #-}

module App.Throughput (Throughput (Throughput), Input, Output, Quantity, Probability) where

import Data.List ((++))
import Data.Maybe (Maybe (Just, Nothing))
import App.Resource (Resource)
import Prelude (Double, Integer, Show, round, show, (*), (.), (/))
import GHC.Generics(Generic)
import Data.Yaml (FromJSON)

data Throughput = Throughput Resource Quantity
                | ThroughputProbabilistic Resource Quantity Probability
                deriving Generic

instance FromJSON Throughput

type Input = Throughput

type Output = Throughput

type Quantity = Double

type Probability = Double
