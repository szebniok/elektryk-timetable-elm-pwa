module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Substitutions.View
import Timetable.View
import TouchEvents
import Types exposing (..)


rootView : Model -> Html Msg
rootView model =
    div [ TouchEvents.onTouchEvent TouchEvents.TouchStart TouchStart, TouchEvents.onTouchEvent TouchEvents.TouchEnd TouchEnd ]
        [ page model
        , navigation model.page
        ]


page : Model -> Html Msg
page model =
    case model.page of
        TimetablePage ->
            Timetable.View.root model

        SubstitutionsPage ->
            Substitutions.View.root model


navigation : Page -> Html Msg
navigation page =
    let
        getClass linkPage =
            if page == linkPage then
                "active"
            else
                ""
    in
    nav []
        [ a [ onClick (SetPage TimetablePage), class (getClass TimetablePage) ] [ text "Plan lekcji" ]
        , a [ onClick (SetPage SubstitutionsPage), class (getClass SubstitutionsPage) ] [ text "Zastępstwa" ]
        ]
