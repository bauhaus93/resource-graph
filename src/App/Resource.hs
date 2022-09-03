{-# LANGUAGE DeriveGeneric #-}

module App.Resource
  ( Resource(Resource)
  ) where

import           Data.List                      ( map )
import           Data.Yaml                      ( FromJSON )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( (<=)
                                                , (==)
                                                , Eq
                                                , Ord
                                                , Show
                                                , String
                                                , show
                                                )

newtype Resource = Resource String
  deriving Generic

instance FromJSON Resource


instance Show Resource where
  show (Resource name) = name

instance Eq Resource where
  (==) (Resource a) (Resource b) = a == b


instance Ord Resource where
  (<=) (Resource a) (Resource b) = a <= b
