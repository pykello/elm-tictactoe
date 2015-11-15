module TicTacToe.Update
  (update)
  where

import TicTacToe.Model exposing (..)
import TicTacToe.Grid as Grid exposing (..)

{-| Update the game model based on the given action. -}
update action model =
  case action of
    Reset ->
      init model.size
    ClickCell x y ->
      if (not (is_game_over model) && (valid_move model.grid x y)) then
         click_cell model x y
      else
         model
         
click_cell model x y =
  {
    grid = 
      Grid.map
        (\cell xx yy -> if (xx, yy) == (x, y) then model.player else cell)
        model.grid,
    player = if model.player == "X" then "O" else "X",
    size = model.size
  }

valid_move grid x y =
  case Grid.get grid x y of
    Just value ->
      value == " "
    Nothing ->
      False
