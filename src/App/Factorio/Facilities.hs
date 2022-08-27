{-# LANGUAGE Safe #-}

module App.Factorio.Facilities (get_facilities) where

import App.Facility as Facility (Facility (Facility))

get_facilities :: [Facility]
get_facilities =
  [ Facility "Stone Furnance" 1.0 (Just "furnance"),
    Facility "Steel Furnance" 2.0 (Just "furnance"),
    Facility "Electric Furnance" 2.0 (Just "furnance"),
    Facility "Assembly MK 1" 0.5 (Just "assembly"),
    Facility "Assembly MK 2" 0.75 (Just "assembly"),
    Facility "Assembly MK 3" 1.25 (Just "assembly"),
    Facility "Oil Refinery" 1.0 (Just "refinery"),
    Facility "Chemical Plant" 1.0 (Just "chemical"),
    Facility "Centrifuge" 1.0 (Just "centrifuge")
  ]
