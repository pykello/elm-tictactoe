module TicTacToe.Util where

import List exposing (..)

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
