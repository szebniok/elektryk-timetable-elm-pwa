module Main exposing (main)

import Html exposing (..)
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.rootView
        }
