module Settings.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode
import Timetable.Types exposing (Class)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ button [ onClick DownloadClasses ] [ text "Pobierz klasy" ]
        , br [] []
        , classSelect model.classes
        ]


classSelect : List Class -> Html Msg
classSelect classes =
    select
        [ onSelect SetClass ]
        (List.map (\class -> option [ value (class.id ++ " " ++ class.name) ] [ text class.name ]) classes)



-- https://gist.github.com/chalmagean/c0b2f874bcff728b3db047aa26b4e477


onSelect : (String -> msg) -> Html.Attribute msg
onSelect msg =
    on "change" (Json.Decode.map msg targetValue)
