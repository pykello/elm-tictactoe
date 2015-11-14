module Util where

import List exposing (..)

row grid r =
  (cell grid r 0) ++ (cell grid r 1) ++ (cell grid r 2)

col grid c =
  (cell grid 0 c) ++ (cell grid 1 c) ++ (cell grid 2 c)
  
diag grid =
  (cell grid 0 0) ++ (cell grid 1 1) ++ (cell grid 2 2)
  
rdiag grid =
  (cell grid 0 2) ++ (cell grid 1 1) ++ (cell grid 2 0)

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
