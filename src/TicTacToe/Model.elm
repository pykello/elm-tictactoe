module TicTacToe.Model where

import List exposing (..)
import String exposing (..)
import TicTacToe.Util exposing (..)
import TicTacToe.Grid as Grid exposing (..)

type Action = ClickCell Int Int | Reset

init size = {
  grid = Grid.create size size " ",
  player = "X",
  size = size}

game_over model =
  not ((winner model) == "") &&
  not (draw model)
  
winner model =
  let
    grid = model.grid
    paths = List.concat [rows grid, cols grid, diags grid]
    xpath = List.repeat model.size "X"
    opath = List.repeat model.size "O"
  in
    if (List.member xpath paths) then "X"
    else if (List.member opath paths) then "O"
    else ""

draw model =
  let
    all_cells = List.concat (rows model.grid)
  in
    not (List.member " " all_cells)
