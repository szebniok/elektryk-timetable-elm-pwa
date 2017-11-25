module View exposing (rootView)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
            substitutions model


substitutions : Model -> Html Msg
substitutions model =
    div [ class "page" ]
        [ button [ onClick FetchSubstitutions ] [ text "pobierz" ]
        , table []
            (List.map substitution model.substitutions)
        , if model.online then
            p [] [ text "Jestes online" ]
          else
            p [] [ text "Jestes offline" ]
        ]


substitution : Substitution -> Html Msg
substitution sub =
    case sub of
        Substitution period class ( subject, teacher, classroom ) ( oldSubject, oldTeacher, oldClassroom ) ->
            let
                oldSubjectDisplay =
                    Maybe.withDefault subject oldSubject

                oldTeacherDisplay =
                    Maybe.withDefault teacher oldTeacher

                oldClassroomDisplay =
                    Maybe.withDefault classroom oldClassroom
            in
            tr []
                [ td [] [ text (toString period) ]
                , td [] [ text class.name ]
                , td []
                    [ text oldSubjectDisplay.name
                    , br [] []
                    , text (oldTeacherDisplay.firstname ++ " " ++ oldTeacherDisplay.lastname)
                    , br [] []
                    , text oldClassroomDisplay.name
                    ]
                , td []
                    [ text subject.name
                    , br [] []
                    , text (teacher.firstname ++ " " ++ teacher.lastname)
                    , br [] []
                    , text classroom.name
                    ]
                ]

        _ ->
            text "zredukowane"


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
