module Settings.View exposing (root)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode
import Substitutions.Types exposing (Class)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ button [ onClick DownloadClasses ] [ text "Pobierz klasy" ]
        , br [] []
        , classSelect model.classes
        ]


classSelect : Dict String Class -> Html Msg
classSelect dict =
    select
        [ onSelect SetClass ]
        (List.map (\( id, class ) -> option [ value id ] [ text class.name ]) <| Dict.toList dict)



-- https://gist.github.com/chalmagean/c0b2f874bcff728b3db047aa26b4e477


onSelect : (String -> msg) -> Html.Attribute msg
onSelect msg =
    on "change" (Json.Decode.map msg targetValue)
