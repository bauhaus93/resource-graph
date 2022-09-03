{-# LANGUAGE DeriveGeneric #-}

module App.Factory
  ( Factory(Factory)
  ) where

import           App.Facility                  as Facility
                                                ( Facility )
import           App.Recipe                    as Recipe
                                                ( Recipe
                                                , ThroughputRate
                                                , calculateRates
                                                , getResources
                                                , validCombination
                                                )
import           App.Resource                  as Resource
                                                ( Resource )
import           Data.List                      ( (++)
                                                , foldr
                                                , map
                                                , nub
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( ($)
                                                , (.)
                                                , (<$>)
                                                , (<*>)
                                                , (>>=)
                                                , Show
                                                , String
                                                , concatMap
                                                , show
                                                , uncurry
                                                )

data Factory = Factory
  { facilites :: [Facility]
  , recipes   :: [Recipe]
  }
  deriving Generic

instance FromJSON Factory

instance Show Factory where
  show fac = facility_string ++ resource_string ++ recipe_string ++ rate_string
   where
    show_list :: Show a => String -> [a] -> String
    show_list title elements =
      title ++ "\n" ++ concatMap (\e -> "\t" ++ show e ++ "\n") elements
    facility_string = show_list "*** Facilities ***" $ facilites fac
    recipe_string   = show_list "*** Recipes ***" $ recipes fac
    rate_string =
      show_list "*** Production Rates ***" $ App.Factory.calculateRates fac
    resource_string =
      show_list "*** Resources ***" $ App.Factory.getResources fac

calculateRates :: Factory -> [ThroughputRate]
calculateRates factory =
  [ Recipe.calculateRates rec fac
  | rec <- recipes factory
  , fac <- facilites factory
  , validCombination fac rec
  ]

getResources :: Factory -> [Resource]
getResources factory = nub $ recipes factory >>= Recipe.getResources
