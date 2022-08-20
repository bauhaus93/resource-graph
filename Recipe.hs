module Recipe (Recipe, get_recipes, Recipe.to_string, Recipe.create, add_input, add_output) where

import Facility
import Resource
import Throughput

data Recipe = Recipe
  { name :: Maybe String,
    facility :: Facility,
    production_time :: Float,
    input :: [Throughput],
    output :: [Throughput]
  }

create :: Facility -> Float -> [Throughput] -> [Throughput] -> Recipe
create facility production_time inputs outputs = Recipe {name = Nothing, facility = facility, production_time = production_time / (Facility.to_speed facility), input = inputs, output = outputs}

with_name :: Recipe -> String -> Recipe
with_name recipe name = recipe {name = Just name}

add_input :: Resource -> Float -> Recipe -> Recipe
add_input resource quantity recipe = recipe {input = new_input : (input recipe)}
  where
    new_input = Throughput.create resource quantity

add_output :: Resource -> Float -> Recipe -> Recipe
add_output resource quantity recipe = recipe {output = new_output : (output recipe)}
  where
    new_output = Throughput.create resource quantity

get_recipes :: [Resource] -> (Resource -> [Recipe]) -> [Recipe]
get_recipes resources recipe_fn = (foldr (++) [] . map recipe_fn) resources

to_string :: Recipe -> String
to_string recipe = Facility.to_name (facility recipe) ++ " (" ++ recipe_name ++ (show (production_time recipe)) ++ "s)" ++ ": " ++ tail inputs_string ++ " -> " ++ tail outputs_string
  where
    recipe_name = case name recipe of
      Just name -> name ++ ", "
      Nothing -> ""
    inputs_string = (to_inputs recipe) >>= (\e -> ", " ++ Throughput.to_string e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")
    outputs_string = (to_outputs recipe) >>= (\e -> ", " ++ Throughput.to_string e ++ " (" ++ show (calculate_rate recipe e) ++ "/s)")

to_inputs :: Recipe -> [Throughput]
to_inputs recipe = input recipe

to_outputs :: Recipe -> [Throughput]
to_outputs recipe = output recipe

to_production_time :: Recipe -> Float
to_production_time recipe = production_time recipe

calculate_rate :: Recipe -> Throughput -> Float
calculate_rate recipe throughput = (Throughput.to_quantity throughput) / (to_production_time recipe)
