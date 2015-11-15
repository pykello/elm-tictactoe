module TicTacToe.View
  (view)
  where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import TicTacToe.Model exposing (..)
import TicTacToe.Grid as Grid exposing (..)

view address model =
  div []
  [
    div [div_style] [view_status model],
    div [div_style] [view_grid address model.grid],
    div [div_style] [view_reset address]
  ]

div_style = style [
  ("padding", "5px"),
  ("text-align", "center"),
  ("width", "165px")]

view_status model =
  text (
    case (get_winner model, is_draw model) of
      (Nothing, False) -> model.player ++ "'s turn."
      (Just winner, _) -> winner ++ " wins!"
      (_, True) -> "draw!"
  )

view_grid address grid =
  create_table grid (
    \cell x y ->
       button [
         style [("height", "50px"), ("width", "50px")],
         onClick address (ClickCell x y)
       ] [text cell]
   )
   
view_reset address =
  button [
    style [("padding", "5px")],
    onClick address Reset
  ][text "New Game"]

create_table grid f =
  table []
    (List.map
       (\row -> tr [] row)
       (Grid.map
          (\cell x y -> td [] [f cell x y])
          grid))
