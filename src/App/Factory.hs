{-# LANGUAGE DeriveGeneric #-}

module App.Factory
  ( Factory(Factory)
  , App.Factory.getResources
  , drawFullRecipeGraph
  ) where

import           App.Facility                  as Facility
                                                ( Facility )
import           App.Recipe                    as Recipe
                                                ( Recipe
                                                , getResources
                                                )
import           App.RecipeNode                 ( buildRecipeGraph )
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
                                                )

data Factory = Factory
  { facilites :: [Facility]
  , recipes   :: [Recipe]
  }
  deriving Generic

instance FromJSON Factory

instance Show Factory where
  show fac = facility_string ++ resource_string ++ recipe_string
   where
    show_list :: Show a => String -> [a] -> String
    show_list title elements =
      title ++ "\n" ++ concatMap (\e -> "\t" ++ show e ++ "\n") elements
    facility_string = show_list "*** Facilities ***" $ facilites fac
    recipe_string   = show_list "*** Recipes ***" $ recipes fac
    resource_string =
      show_list "*** Resources ***" $ App.Factory.getResources fac

drawFullRecipeGraph :: Factory -> String
drawFullRecipeGraph factory = wrapIntoDigraph recipe_graph_string
 where
  recipe_nodes        = (buildRecipeGraph . recipes) factory
  recipe_graph_string = concatMap show recipe_nodes


wrapIntoDigraph :: String -> String
wrapIntoDigraph graph = drawDigraphIntro ++ graph ++ drawDigraphOutro

drawDigraphIntro :: String
drawDigraphIntro =
  "digraph resource_graph {\nnode [shape=record];\nfontname=Courier;\nlabelloc=top;\nlabeljust=left;\n"


drawDigraphOutro :: String
drawDigraphOutro = "}"

getResources :: Factory -> [Resource]
getResources factory = nub $ recipes factory >>= Recipe.getResources
