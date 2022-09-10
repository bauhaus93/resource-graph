module App.RecipeNode
  ( buildRecipeGraph
  ) where

import           App.Recipe                     ( Recipe
                                                , getNeededResources
                                                , getProducedResources
                                                , getResources
                                                , supplies
                                                , toName
                                                )
import           App.Resource                   ( Resource )
import           Data.Char                      ( toLower )
import           Data.List                      ( (++)
                                                , filter
                                                , map
                                                )
import           Data.List.Utils                ( replace )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           Data.Set                       ( Set
                                                , difference
                                                , empty
                                                , fromList
                                                , notMember
                                                , null
                                                , toList
                                                )
import           Prelude                        ( ($)
                                                , (.)
                                                , (/=)
                                                , (>>=)
                                                , Bool
                                                , Double
                                                , Show
                                                , String
                                                , concatMap
                                                , not
                                                , show
                                                )


data RecipeNode = RecipeNode Recipe [Recipe]


instance Show RecipeNode where
  show (RecipeNode rec deps) = vertex ++ edges
   where
    vertex = toVertex rec
    edges  = concatMap (toEdge rec) deps


toVertex :: Recipe -> String
toVertex rec =
  name
    ++ " [label=\""
    ++ toName rec
    ++ "\" fillcolor=aliceblue; style=filled]\n"
  where name = toNodeName rec

toEdge :: Recipe -> Recipe -> String
toEdge src dest = toNodeName src ++ " -> " ++ toNodeName dest ++ "\n"

toNodeName :: Recipe -> String
toNodeName recipe =
  map toLower $ (replace "-" "_" . replace " " "_") (toName recipe)

buildRecipeGraph :: [Recipe] -> [RecipeNode]
buildRecipeGraph recipes =
  map (\rec -> RecipeNode rec (supplyingRecipes recipes rec)) recipes

supplyingRecipes :: [Recipe] -> Recipe -> [Recipe]
supplyingRecipes available_recipes dest_recipe =
  filter (`supplies` dest_recipe) available_recipes
