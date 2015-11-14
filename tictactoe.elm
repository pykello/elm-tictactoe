import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import List exposing (..)
import String exposing (..)

main =
  StartApp.start { model = init, view = view, update = update }

init = {
  grid = [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]],
  player = "X"}

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
    w = winner model.grid
  in
    text (
      if (not (w == "")) then w ++ " won!"
      else if (draw model.grid) then "draw!"
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

type Action = ClickCell Int Int | Reset

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
  

row grid r =
  (cell grid r 0) ++ (cell grid r 1) ++ (cell grid r 2)

col grid c =
  (cell grid 0 c) ++ (cell grid 1 c) ++ (cell grid 2 c)
  
diag grid =
  (cell grid 0 0) ++ (cell grid 1 1) ++ (cell grid 2 2)
  
rdiag grid =
  (cell grid 0 2) ++ (cell grid 1 1) ++ (cell grid 2 0)

invalid_move grid x y =
  not ((cell grid x y) == " ")

create_table grid f =
  table []
    (List.map
       (\row -> tr [] row)
       (map_grid
          (\cell x y -> td [] [f cell x y])
          grid))

map_grid f grid =
  map_grid2 f grid 0

map_grid2 f grid x =
  case grid of
     [] ->
       []
     row::rest ->
       (map_row f row x 0)::
       (map_grid2 f rest (x+1))

map_row f row x y =
  case row of
     [] ->
       []
     cell::rest ->
       (f cell x y)::
       (map_row f rest x (y+1))

cell grid x y =
  let
    row  = 
      head (drop x grid)
    cell =
      case row of
        Just r -> head (drop y r)
        Nothing -> Nothing
  in
    case cell of
      Just c -> c
      Nothing -> ""
