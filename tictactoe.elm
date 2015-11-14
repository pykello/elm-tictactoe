module TicTacToe where

import StartApp.Simple as StartApp
import Model exposing (init)
import View exposing (view)
import Update exposing (update)

main =
  StartApp.start { model = init 4, view = view, update = update }
