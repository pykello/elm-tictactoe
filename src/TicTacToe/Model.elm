module TicTacToe.Model where

import List exposing (..)
import String exposing (..)
import TicTacToe.Grid as Grid exposing (..)

type Action = ClickCell Int Int | Reset
type alias Model = {grid: Grid String, player: String, size: Int}

{-| Initial game model. -}
init: Int -> Model
init size = {
  grid = Grid.create size size " ",
  player = "X",
  size = size}

{-| Is game over? -}
is_game_over: Model -> Bool
is_game_over model =
  case (get_winner model, is_draw model) of
    (Nothing, False) -> False
    (_, _) -> True

{-| Returns winner of the game (Just "X" or Just "O"). If no one has won yet,
returns Nothing.
-}
get_winner: Model -> Maybe String
get_winner model =
  let
    grid = model.grid
    paths = List.concat [rows grid, cols grid, [diag grid, rdiag grid]]
    xpath = List.repeat model.size "X"
    opath = List.repeat model.size "O"
  in
    if List.member xpath paths then Just "X"
    else if List.member opath paths then Just "O"
    else Nothing

{-| Did game end in draw? -}
is_draw: Model -> Bool
is_draw model =
  let
    all_cells = List.concat (rows model.grid)
  in
    not (List.member " " all_cells)
