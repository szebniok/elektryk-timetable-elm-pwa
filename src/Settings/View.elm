module Settings.View exposing (root)

import Html exposing (..)
import Html.Events exposing (onClick)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ button [ onClick DownloadClasses ] [ text "Pobierz klasy" ]
        , br [] []
        , text (toString model.classes)
        ]
