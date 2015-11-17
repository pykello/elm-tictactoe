module TicTacToe.View
  (view)
  where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import TicTacToe.Model exposing (..)
import TicTacToe.Grid as Grid exposing (..)

{-| Main game view. -}
view address model =
  div []
  [
    div [div_style] [view_status model],
    div [div_style] [view_grid address model.grid],
    div [div_style] [view_reset_button address]
  ]

{-| Game status view. -}
view_status model =
  text (
    case (get_winner model, is_draw model) of
      (Nothing, False) -> model.player ++ "'s turn."
      (Just winner, _) -> winner ++ " wins!"
      (_, True) -> "draw!"
  )

{-| Game grid view. -}
view_grid address grid =
  let
    cell_view = \x y cell ->
                  td[] [
                    button [
                      cell_button_style,
                      onClick address (ClickCell x y)
                    ] [text cell]
                  ]
    cell_grid = Grid.map cell_view grid
    rows = List.map (\row -> tr [] row) cell_grid
  in
    table [] rows

{-| Reset button view. -}
view_reset_button address =
  button [
    reset_button_style,
    onClick address Reset
  ][text "New Game"]

{-| Styles. -}
div_style = style [
  ("padding", "5px"),
  ("text-align", "center"),
  ("width", "165px")]

cell_button_style = style [
  ("height", "50px"),
  ("width", "50px")]

reset_button_style = style [
  ("padding", "5px")]
