module TicTacToe where

import StartApp.Simple as StartApp
import TicTacToe.Model exposing (init)
import TicTacToe.View exposing (view)
import TicTacToe.Update exposing (update)

main =
  StartApp.start { model = init 4, view = view, update = update }
