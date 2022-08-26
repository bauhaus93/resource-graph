{-# LANGUAGE Safe #-}

module Recipe (Recipe, RecipeComparableByOutput, calculate_factor_for_rate, to_comparable_by_output, get_recipes, Recipe.create, add_input, add_output, to_inputs, to_output, to_outputs, to_output_resources, calculate_resource_output_rate) where

import Data.List (foldr, head, map, tail, (++))
import Data.Maybe (Maybe (Just, Nothing))
import Facility (Facility, to_name, to_speed)
import Resource (Resource)
import Throughput (Throughput, create, to_quantity, to_resource)
import Prelude (Eq, Float, Integer, Ord, Show, String, abs, ceiling, show, (-), (.), (/), (<$>), (<=), (==), (>>=))

data Recipe = Recipe
  { name :: Maybe String,
    facility :: Facility,
    production_time :: Float,
    input :: [Throughput],
    output :: [Throughput]
  }

instance Show Recipe where
  show recipe = Facility.to_name (facility recipe) ++ " (" ++ recipe_name ++ (show (production_time recipe)) ++ "s)" ++ ": " ++ tail inputs_string ++ " -> " ++ tail outputs_string
    where
      recipe_name = case name recipe of
        Just n -> n ++ ", "
        Nothing -> ""
      inputs_string = (to_inputs recipe) >>= (\e -> ", " ++ show e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")
      outputs_string = (to_outputs recipe) >>= (\e -> ", " ++ show e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")

data RecipeComparableByOutput = RecipeComparableByOutput Resource Recipe

instance Ord RecipeComparableByOutput where
  (<=) (RecipeComparableByOutput a_res a_rec) (RecipeComparableByOutput b_res b_rec) = (calculate_resource_output_rate a_res a_rec) <= (calculate_resource_output_rate b_res b_rec)

instance Eq RecipeComparableByOutput where
  (==) (RecipeComparableByOutput a_res a_rec) (RecipeComparableByOutput b_res b_rec) =
    abs (output_a - output_b) <= 0.003
    where
      output_a = calculate_resource_output_rate a_res a_rec
      output_b = calculate_resource_output_rate b_res b_rec

to_comparable_by_output :: Resource -> Recipe -> RecipeComparableByOutput
to_comparable_by_output = RecipeComparableByOutput

create :: Facility -> Float -> [Throughput] -> [Throughput] -> Recipe
create recipe_facility recipe_production_time inputs outputs = Recipe {name = Nothing, facility = recipe_facility, production_time = recipe_production_time / (Facility.to_speed recipe_facility), input = inputs, output = outputs}

add_input :: Resource -> Float -> Recipe -> Recipe
add_input resource quantity recipe = recipe {input = new_input : (to_inputs recipe)}
  where
    new_input = Throughput.create resource quantity

add_output :: Resource -> Float -> Recipe -> Recipe
add_output resource quantity recipe = recipe {output = new_output : (to_outputs recipe)}
  where
    new_output = Throughput.create resource quantity

get_recipes :: [Resource] -> (Resource -> [Recipe]) -> [Recipe]
get_recipes resources recipe_fn = (foldr (++) [] . map recipe_fn) resources

calculate_factor_for_rate :: Resource -> Float -> Recipe -> Maybe Integer
calculate_factor_for_rate resource target_rate recipe = (ceiling . (\q -> target_rate / (q / time)) . Throughput.to_quantity) <$> Recipe.to_output resource recipe
  where
    time = Recipe.to_production_time recipe

to_inputs :: Recipe -> [Throughput]
to_inputs recipe = input recipe

to_outputs :: Recipe -> [Throughput]
to_outputs recipe = output recipe

to_output :: Resource -> Recipe -> Maybe Throughput
to_output wanted_resource recipe = case outputs_with_resource of
  [] -> Nothing
  xs -> Just (head xs)
  where
    outputs_with_resource = [out | out <- Recipe.to_outputs recipe, Throughput.to_resource out == wanted_resource]

to_output_resources :: Recipe -> [Resource]
to_output_resources recipe = map Throughput.to_resource (Recipe.to_outputs recipe)

calculate_resource_output_rate :: Resource -> Recipe -> Float
calculate_resource_output_rate target_resource recipe = (calculate_rate recipe . head) [tp | tp <- to_outputs recipe, Throughput.to_resource tp == target_resource]

to_production_time :: Recipe -> Float
to_production_time recipe = production_time recipe

calculate_rate :: Recipe -> Throughput -> Float
calculate_rate recipe throughput = (Throughput.to_quantity throughput) / (to_production_time recipe)
