{-# LANGUAGE Safe #-}

module ProductionChain (Node, get_node, ProductionChain.to_inputs, to_recipe) where

import Data.List (elem, head, reverse, sortOn, (++))
import Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import Factory (Factory, to_recipes)
import Recipe (Recipe, calculate_factor_for_rate, to_comparable_by_output, to_inputs, to_output_resources)
import Resource (Resource)
import Throughput (Throughput, multiply, to_quantity, to_resource)
import Prelude (Float, Integer, Show, fromIntegral, show, ($), (.), (<$>), (<*>), (>>=))

data Node = Node
  { recipe :: Recipe,
    amount :: Integer,
    inputs :: [Node]
  }

instance Show Node where
  show node = show recipe_str ++ "\n" ++ inputs_str
    where
      recipe_str = ((show . to_amount) node) ++ " x " ++ ((show . to_recipe) node)
      inputs_str = case (ProductionChain.to_inputs node) of
        [] -> ""
        xs -> xs >>= show

get_node :: Factory -> Resource -> Float -> Maybe Node
get_node factory target_resource target_rate = to_node <$> maybe_recipe <*> maybe_factor
  where
    to_node :: Recipe -> Integer -> Node
    to_node used_recipe factor =
      Node
        { recipe = used_recipe,
          amount = factor,
          inputs =
            mapMaybe
              ( to_input_node
                  . to_res_quant_pair
                  . Throughput.multiply (fromIntegral factor)
              )
              $ Recipe.to_inputs used_recipe
        }
    to_res_quant_pair :: Throughput -> (Resource, Float)
    to_res_quant_pair throughput = (Throughput.to_resource throughput, Throughput.to_quantity throughput)
    to_input_node :: (Resource, Float) -> Maybe Node
    to_input_node (res, rate) = get_node factory res rate
    available_recipes = Factory.to_recipes factory
    maybe_recipe = find_best_resource_recipe available_recipes target_resource
    maybe_factor = maybe_recipe >>= Recipe.calculate_factor_for_rate target_resource target_rate

to_recipe :: Node -> Recipe
to_recipe node = recipe node

to_inputs :: Node -> [Node]
to_inputs = inputs

to_amount :: Node -> Integer
to_amount = amount

find_best_resource_recipe :: [Recipe] -> Resource -> Maybe Recipe
find_best_resource_recipe recipes target_resource = case available_recipes of
  [] -> Nothing
  xs -> (Just . head . reverse . resource_sorter) xs
  where
    available_recipes = find_recipes_for_resource recipes target_resource
    resource_sorter :: [Recipe] -> [Recipe]
    resource_sorter = sort_recipes_by_resource_output target_resource

find_recipes_for_resource :: [Recipe] -> Resource -> [Recipe]
find_recipes_for_resource recipes target_resource = [r | r <- recipes, elem target_resource (Recipe.to_output_resources r)]

sort_recipes_by_resource_output :: Resource -> [Recipe] -> [Recipe]
sort_recipes_by_resource_output target_resource recipes = sortOn (Recipe.to_comparable_by_output target_resource) recipes
