module Util where

import List exposing (..)

rows grid =
  map (\i -> row grid i) (size_range grid)

cols grid =
  map (\i -> col grid i) (size_range grid)

diags grid =
  [diag grid, rdiag grid]

row grid r =
  path (\i -> (r, i)) grid

col grid c =
  path (\i -> (i, c)) grid
  
diag grid =
  path (\i -> (i, i)) grid
  
rdiag grid =
  let
    n = List.length grid
  in
    path (\i -> (i, n - 1 - i)) grid

path f grid =
  foldr (++) ""
    (map
       (\i -> let (x, y) = f(i) in grid_get_def grid x y)
       (size_range grid))

range a b =
  if a > b then []
  else a :: (range (a + 1) b)

size_range grid =
  range 0 (List.length grid - 1)

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
  grid_get_def grid x y

list_get list x =
  head (drop x list)

grid_get grid x y =
  case list_get grid x of
    Just r -> list_get r y
    Nothing -> Nothing

grid_get_def grid x y =
  case grid_get grid x y of
    Just c -> c
    Nothing -> ""
