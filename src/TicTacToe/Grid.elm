module TicTacToe.Grid
  (Grid, create, rows, cols, diag, rdiag, get, map)
  where

import List exposing (..)

type alias Grid a = List (List a)

{-| Create a grid of given size filled with the given value.
For example, `create 3 3 "."` would return:
    [[".", ".", "."],
     [".", ".", "."],
     [".", ".", "."]]
-}
create: Int -> Int -> a -> Grid a
create rows cols fill =
  List.repeat rows (List.repeat cols fill)

{-| Get list of rows of the given grid. -}
rows: Grid a -> List (List a)
rows grid =
  grid

{-| Get list of columns of the given grid. -}
cols: Grid a -> List (List a)
cols grid =
  let
    first_col = List.filterMap List.head grid
    rest = List.filterMap List.tail grid
  in
    if first_col == [] then
      []
    else
      first_col :: cols rest

{-| Get the list of elements in the diagonal of the grid.
i.e. all elements with indexes (i, i).
-}
diag: Grid a -> List a
diag grid =
  let
    rest = List.drop 1 (List.filterMap List.tail grid)
  in
    case get grid 0 0 of
      Just r ->
        r :: diag rest
      Nothing ->
        []

{-| Get the list of elements in the reverse diagonal of the grid.
i.e. all elements with indexes (i, cols - 1 - i).
-}
rdiag: Grid a -> List a
rdiag grid =
  diag (List.map List.reverse grid)

{-| Get the element with the given indexes in the grid. -}
get: Grid a -> Int -> Int -> Maybe a
get grid x y =
  case List.head (List.drop x grid) of
    Just r -> List.head (List.drop y r)
    Nothing -> Nothing

{-| Apply the given (value, row, col) -> mapped_value function to the
elements of the grid.
-}
map: (a -> Int -> Int -> b) -> Grid a -> (List (List b))
map f grid =
  map_ f grid 0

map_ f grid x =
  case grid of
     [] ->
       []
     row::rest ->
       (map_row_ f row x 0)::
       (map_ f rest (x+1))

map_row_ f row x y =
  case row of
     [] ->
       []
     cell::rest ->
       (f cell x y)::
       (map_row_ f rest x (y+1))
