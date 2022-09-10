{-# LANGUAGE DeriveGeneric #-}

module App.Recipe
  ( Recipe
  , getResources
  , getProducedResources
  , getNeededResources
  , supplies
  , App.Recipe.toName
  ) where

import           App.Facility                   ( Facility
                                                , FacilityType
                                                , toFacilityType
                                                , toName
                                                , toSpeed
                                                )
import           App.Resource                   ( Resource )
import           App.Throughput                as Throughput
                                                ( Input
                                                , Output
                                                , Throughput
                                                , multiply
                                                , toResource
                                                )
import           Data.List                      ( (++)
                                                , intercalate
                                                , map
                                                , nub
                                                , sort
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing)
                                                , maybe
                                                )
import           Data.Set                       ( fromList
                                                , intersection
                                                , null
                                                , toList
                                                )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( ($)
                                                , (&&)
                                                , (.)
                                                , (/)
                                                , (==)
                                                , Bool
                                                , Double
                                                , Eq
                                                , Integer
                                                , Show
                                                , String
                                                , not
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


instance Eq Recipe where
  (==) l r = name l == name r

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

supplies :: Recipe -> Recipe -> Bool
supplies src_recipe dest_recipe = (not . null) $ intersection
  ((fromList . getNeededResources) dest_recipe)
  ((fromList . getProducedResources) src_recipe)

getResources :: Recipe -> [Resource]
getResources recipe =
  nub $ getProducedResources recipe ++ getNeededResources recipe

toName :: Recipe -> String
toName = name

getProducedResources :: Recipe -> [Resource]
getProducedResources recipe = map toResource $ input recipe

getNeededResources :: Recipe -> [Resource]
getNeededResources recipe = map toResource $ output recipe
