module Main exposing (main)

import Navigation
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.rootView
        }
