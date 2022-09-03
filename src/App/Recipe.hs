{-# LANGUAGE DeriveGeneric #-}

module App.Recipe (Recipe, RecipeOutputOrder (RecipeOutputOrder), calculate_factor_for_rate, get_recipes, add_input, add_output, to_inputs, to_output, to_outputs, to_output_resources, calculate_resource_output_rate, to_facility, create) where

import Data.List (foldr, head, map, tail, (++))
import Data.Maybe (Maybe (Just, Nothing))
import App.Facility as Facility (Facility, to_name, to_speed)
import App.Resource as Resource (Resource)
import App.Throughput as Throughput (Input, Output, Throughput (Throughput), to_quantity, to_resource)
import Prelude (Eq, Float, Integer, Ord, Show, String, abs, ceiling, show, ($), (-), (.), (/), (<$>), (<=), (==), (>>=))
import GHC.Generics(Generic)
import Data.Yaml (FromJSON)

data Recipe = Recipe
  { name :: Maybe String,
    facility :: Facility,
    production_time :: Float,
    input :: [Input],
    output :: [Output]
  } deriving Generic


instance FromJSON Recipe

instance Show Recipe where
  show recipe = Facility.to_name (facility recipe) ++ " (" ++ recipe_name ++ (show (production_time recipe)) ++ "s)" ++ ": " ++ tail inputs_string ++ " -> " ++ tail outputs_string
    where
      recipe_name = case name recipe of
        Just n -> n ++ ", "
        Nothing -> ""
      inputs_string = (to_inputs recipe) >>= (\e -> ", " ++ show e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")
      outputs_string = (to_outputs recipe) >>= (\e -> ", " ++ show e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")

data RecipeOutputOrder = RecipeOutputOrder Resource Recipe

instance Ord RecipeOutputOrder where
  (<=) (RecipeOutputOrder a_res a_rec) (RecipeOutputOrder b_res b_rec) = (calculate_resource_output_rate a_res a_rec) <= (calculate_resource_output_rate b_res b_rec)

instance Eq RecipeOutputOrder where
  (==) (RecipeOutputOrder a_res a_rec) (RecipeOutputOrder b_res b_rec) =
    abs (output_a - output_b) <= 0.003
    where
      output_a = calculate_resource_output_rate a_res a_rec
      output_b = calculate_resource_output_rate b_res b_rec

create :: Facility -> Float -> [Throughput] -> [Throughput] -> Recipe
create recipe_facility recipe_production_time inputs outputs = Recipe {name = Nothing, facility = recipe_facility, production_time = recipe_production_time / (Facility.to_speed recipe_facility), input = inputs, output = outputs}

add_input :: Resource -> Float -> Recipe -> Recipe
add_input res quant recipe = recipe {input = new_input : (to_inputs recipe)}
  where
    new_input = Throughput res quant Nothing

add_output :: Resource -> Float -> Recipe -> Recipe
add_output res quant recipe = recipe {output = new_output : (to_outputs recipe)}
  where
    new_output = Throughput res quant Nothing

get_recipes :: [Resource] -> (Resource -> [Recipe]) -> [Recipe]
get_recipes resources recipe_fn = foldr (++) [] . map recipe_fn $ resources

calculate_factor_for_rate :: Resource -> Float -> Recipe -> Maybe Integer
calculate_factor_for_rate resource target_rate recipe = (ceiling . (\q -> target_rate / (q / time)) . Throughput.to_quantity) <$> to_output resource recipe
  where
    time = to_production_time recipe

to_inputs :: Recipe -> [Throughput]
to_inputs recipe = input recipe

to_outputs :: Recipe -> [Throughput]
to_outputs recipe = output recipe

to_output :: Resource -> Recipe -> Maybe Throughput
to_output wanted_resource recipe = case outputs_with_resource of
  [] -> Nothing
  xs -> Just (head xs)
  where
    outputs_with_resource = [out | out <- to_outputs recipe, Throughput.to_resource out == wanted_resource]

to_facility :: Recipe -> Facility
to_facility = facility

to_output_resources :: Recipe -> [Resource]
to_output_resources recipe = map Throughput.to_resource $ to_outputs recipe

calculate_resource_output_rate :: Resource -> Recipe -> Float
calculate_resource_output_rate target_resource recipe = calculate_rate recipe . head $ [tp | tp <- to_outputs recipe, Throughput.to_resource tp == target_resource]

to_production_time :: Recipe -> Float
to_production_time recipe = production_time recipe

calculate_rate :: Recipe -> Throughput -> Float
calculate_rate recipe throughput = (Throughput.to_quantity throughput) / (to_production_time recipe)
