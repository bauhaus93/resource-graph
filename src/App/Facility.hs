{-# LANGUAGE DeriveGeneric #-}

module App.Facility
  ( Facility(Facility)
  , FacilityType
  , toSpeed
  , toName
  , toFacilityType
  ) where

import           Data.List                      ( (++)
                                                , map
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( (.)
                                                , (==)
                                                , Bool
                                                , Double
                                                , Show
                                                , String
                                                , show
                                                )

data Facility = Facility
  { name          :: String
  , speed         :: Double
  , facility_type :: FacilityType
  }
  deriving Generic

instance FromJSON Facility

type FacilityType = String


instance Show Facility where
  show fac =
    name fac
      ++ " (x"
      ++ (show . toSpeed) fac
      ++ ", "
      ++ facility_type fac
      ++ ")"


toSpeed :: Facility -> Double
toSpeed = speed


toName :: Facility -> String
toName = name

toFacilityType :: Facility -> FacilityType
toFacilityType = facility_type

