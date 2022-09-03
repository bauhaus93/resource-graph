{-# LANGUAGE DeriveGeneric #-}

module App.Recipe
  ( Recipe
  , ThroughputRate
  , calculateRates
  , validCombination
  ) where

import           App.Facility                   ( Facility
                                                , FacilityType
                                                , toFacilityType
                                                , toName
                                                , toSpeed
                                                )
import           App.Throughput                as Throughput
                                                ( Input
                                                , Output
                                                , Throughput
                                                , multiply
                                                )
import           Data.List                      ( (++)
                                                , intercalate
                                                , map
                                                )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( ($)
                                                , (.)
                                                , (/)
                                                , (==)
                                                , Bool
                                                , Double
                                                , Eq
                                                , Integer
                                                , Show
                                                , String
                                                , show
                                                )

data Recipe = Recipe
  { name            :: String
  , facility_type   :: FacilityType
  , production_time :: Double
  , input           :: [Input]
  , output          :: [Output]
  }
  deriving Generic

instance FromJSON Recipe

instance Show Recipe where
  show rec =
    name rec
      ++ " ("
      ++ (show . production_time) rec
      ++ "s): "
      ++ input_str
      ++ " ==> "
      ++ output_str
   where
    input_str  = intercalate ", " $ map show $ input rec
    output_str = intercalate ", " $ map show $ output rec


data ThroughputRate = ThroughputRate
  { recipe      :: Recipe
  , facility    :: Facility
  , input_rate  :: [Input]
  , output_rate :: [Output]
  }


instance Show ThroughputRate where
  show rate =
    recipe_name
      ++ " @ "
      ++ facility_name
      ++ ": "
      ++ input_str
      ++ " ==> "
      ++ output_str
   where
    recipe_name   = (name . recipe) rate
    facility_name = (toName . facility) rate
    input_str     = intercalate ", " $ map show $ input_rate rate
    output_str    = intercalate ", " $ map show $ output_rate rate

calculateRates :: Recipe -> Facility -> ThroughputRate
calculateRates rec fac = ThroughputRate { recipe      = rec
                                        , facility    = fac
                                        , input_rate  = rate_map $ input rec
                                        , output_rate = rate_map $ output rec
                                        }
 where
  rate_map :: [Throughput] -> [Throughput]
  rate_map = map (multiply $ 1 / (production_time rec / toSpeed fac))


validCombination :: Facility -> Recipe -> Bool
validCombination facility recipe =
  toFacilityType facility == facility_type recipe
