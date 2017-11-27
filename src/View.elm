module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Decode
import Substitutions.View
import Timetable.View
import Types exposing (..)


rootView : Model -> Html Msg
rootView model =
    div []
        [ page model
        , navigation model.page
        ]


page : Model -> Html Msg
page model =
    case model.page of
        TimetablePage ->
            Timetable.View.root model.timetable
                |> Html.map TimetableMsg

        SubstitutionsPage ->
            Substitutions.View.root model.substitutions
                |> Html.map SubstitutionsMsg

        NotFoundPage ->
            p [] [ text "Żądana strona nie została znaleziona" ]


navigation : Page -> Html Msg
navigation page =
    let
        getClass linkPage =
            if page == linkPage then
                "active"
            else
                ""

        navigationLink page content =
            a
                [ onWithOptions
                    "click"
                    { stopPropagation = False
                    , preventDefault = True
                    }
                    (Debug.log "debug 51:" (Decode.succeed (SetPage page)))
                , class (getClass page)
                , href (reversePage page)
                ]
                [ text content ]
    in
    nav []
        [ navigationLink TimetablePage "Plan lekcji"
        , navigationLink SubstitutionsPage "Zastępstwa"
        ]
