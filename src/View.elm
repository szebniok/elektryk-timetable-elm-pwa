module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick, onWithOptions)
import Json.Decode as Decode
import Settings.View
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

        SettingsPage ->
            Settings.View.root model

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
                    (Decode.succeed (SetPage page))
                , class (getClass page)
                , href (reversePage page)
                ]
                [ text content ]
    in
    nav []
        [ navigationLink TimetablePage "Plan lekcji"
        , navigationLink SubstitutionsPage "Zastępstwa"
        , navigationLink SettingsPage "Ustawienia"
        ]
