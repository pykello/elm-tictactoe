module Model where

import List exposing (..)
import String exposing (..)
import Util exposing (..)

type Action = ClickCell Int Int | Reset

init size = {
  grid = List.repeat size (List.repeat size " "),
  player = "X",
  size = size}

game_over model =
  not ((winner model) == "") &&
  not (draw model)
  
winner model =
  let
    grid = model.grid
    paths = List.concat [rows grid, cols grid, diags grid]
    xpath = String.repeat model.size "X"
    opath = String.repeat model.size "O"
  in
    if (List.member xpath paths) then "X"
    else if (List.member opath paths) then "O"
    else ""

draw model =
  let
    all_cells = String.concat (rows model.grid)
  in
    not (String.contains " " all_cells)
