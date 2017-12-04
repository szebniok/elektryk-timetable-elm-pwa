module Settings.View exposing (root)

import Html exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div [] [ text "Tutaj pojawi się możliwość ustawienia klasy, zrestartowania danych lokalnych i service workera" ]
