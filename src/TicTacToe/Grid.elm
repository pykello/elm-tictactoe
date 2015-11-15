module TicTacToe.Grid where

import List exposing (..)

type alias Grid a = List (List a)

create: Int -> Int -> a -> Grid a
create rows cols fill =
  List.repeat rows (List.repeat cols fill)

rows: Grid a -> List (List a)
rows grid =
  grid

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

rdiag: Grid a -> List a
rdiag grid =
  diag (vflip grid)

diags: Grid a -> List (List a)
diags grid =
  [diag grid, rdiag grid]

vflip: Grid a -> Grid a
vflip grid =
  List.map List.reverse grid

get: Grid a -> Int -> Int -> Maybe a
get grid x y =
  case List.head (List.drop x grid) of
    Just r -> List.head (List.drop y r)
    Nothing -> Nothing
