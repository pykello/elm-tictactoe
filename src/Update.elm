module Update where

import Model exposing (..)
import Util exposing (..)

update action model =
  case action of
    Reset ->
      init
    ClickCell x y ->
      if (not (game_over model.grid) &&
          not (invalid_move model.grid x y))
      then
         play model x y
      else
         model
         
play model x y =
  {
    grid = 
      map_grid
        (\cell xx yy -> if (xx, yy) == (x, y) then model.player else cell)
        model.grid,
    player = if model.player == "X" then "O" else "X"
  }

invalid_move grid x y =
  not ((cell grid x y) == " ")
