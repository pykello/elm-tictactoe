module TicTacToe.Update
  (update)
  where

import TicTacToe.Model exposing (..)
import TicTacToe.Grid as Grid exposing (..)

{-| Update the game model based on the given action. -}
update: Action -> Model -> Model
update action model =
  case (action, is_game_over model) of
    (Reset, _) ->
      init model.size
    (_, True) ->
      model
    (ClickCell x y, False) ->
      if valid_move model.grid x y then
        {
          grid = Grid.set model.grid x y model.player,
          player = if model.player == "X" then "O" else "X",
          size = model.size
        }
      else
         model

{-| Is clicking on the given cell allowed? -}
valid_move: Grid String -> Int -> Int -> Bool
valid_move grid x y =
  case Grid.get grid x y of
    Just value ->
      value == " "
    Nothing ->
      False
