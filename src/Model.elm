module Model where

import List exposing (..)
import String exposing (..)
import Util exposing (..)

type Action = ClickCell Int Int | Reset

init = {
  grid = [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]],
  player = "X"}

game_over grid =
  not ((winner grid) == "") &&
  not (draw grid)
  
winner grid =
  let
    paths = [
      (row grid 0), (row grid 1), (row grid 2),
      (col grid 0), (col grid 1), (col grid 2),
      (diag grid), (rdiag grid)
    ]
  in
    if (List.member "XXX" paths) then "X"
    else if (List.member "OOO" paths) then "O"
    else ""

draw grid =
  not (String.contains " "
    ((row grid 0) ++ (row grid 1) ++ (row grid 2)))
