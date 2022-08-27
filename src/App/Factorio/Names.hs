{-# LANGUAGE Safe #-}

module App.Factorio.Names (get_resource_names) where

import Data.List (foldr, (++))
import Prelude (String)

get_resource_names :: [String]
get_resource_names = foldr (++) [] [ore_names, fluid_names, intermediate_names]

ore_names :: [String]
ore_names = ["Wood", "Iron Ore", "Copper Ore", "Stone Ore", "Coal", "Uranium Ore"]

intermediate_names :: [String]
intermediate_names =
  [ "Iron Plate",
    "Copper Plate",
    "Solid Fuel",
    "Steel Plate",
    "Plastic Bar",
    "Sulfur",
    "Battery",
    "Explosives",
    "Copper Cable",
    "Iron Stick",
    "Iron Gear Wheel",
    "Electronic Circuit",
    "Advanced Circuit",
    "Processing Unit",
    "Engine Unit",
    "Electric Engine Unit",
    "Flying Robot Frame",
    "Rocket Part",
    "Rocket Control Unit",
    "Low Density Structure",
    "Rocket Fuel",
    "Nuclear Fuel",
    "Uranium-235",
    "Uranium-238",
    "Uranium Fuel Cell",
    "Used Up Uranium Fuel Cell"
  ]

fluid_names :: [String]
fluid_names = ["Crude Oil", "Heavy Oil", "Light Oil", "Lubricant", "Petroleum Gas", "Sulfuric Acid", "Water", "Steam"]
