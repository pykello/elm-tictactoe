module View where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Model exposing (..)
import Util exposing (..)

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
  let
    w = winner model
  in
    text (
      if (not (w == "")) then w ++ " won!"
      else if (draw model) then "draw!"
      else model.player ++ "'s turn."
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
       (map_grid
          (\cell x y -> td [] [f cell x y])
          grid))
