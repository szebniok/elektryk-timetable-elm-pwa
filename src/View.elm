module View exposing (rootView)

import Array
import Date.Extra.Facts exposing (dayOfWeekFromWeekdayNumber)
import Date.Extra.I18n.I_pl_pl exposing (dayName)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Parser exposing (Lesson(Empty, Lesson), Substitution(Substitution), Timetable, TimetableCell(Lessons, NoLessons), TimetableRow, parse, substitutionsParser)
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
            timetable model

        SubstitutionsPage ->
            substitutions model


timetable : Model -> Html Msg
timetable model =
    div [ class "page" ]
        [ h2 [ class "day-of-week" ]
            [ text (dayName (dayOfWeekFromWeekdayNumber (model.currentDayIndex + 1))) ]
        , displayTable model.currentDayIndex model.data
        , if model.online then
            button [ onClick Update ] [ text "Pobierz nowa zawartosc" ]
          else
            p [] [ text "Jestes offline" ]
        ]


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


displayTable : Int -> Timetable -> Html msg
displayTable index timetable =
    div []
        [ tableRow (Maybe.withDefault [] (Array.get index timetable)) ]


tableRow : TimetableRow -> Html msg
tableRow row =
    div [ class "timetable-row" ]
        (List.indexedMap tableCell row)


tableCell : Int -> TimetableCell -> Html msg
tableCell index cell =
    case cell of
        Lessons lessons ->
            div [ class "timetable-cell" ]
                ([ div [ class "timetable-cell-index" ] [ text (toString index) ] ]
                    ++ List.map displayLesson lessons
                )

        -- if there are no lessons in cell at all, return empty node
        NoLessons ->
            text ""


displayLesson : Lesson -> Html msg
displayLesson lesson =
    let
        go : List (Html msg)
        go =
            case lesson of
                Lesson { subject, teacher, classroom } ->
                    [ text subject.name
                    , br [] []
                    , text (teacher.firstname ++ " " ++ teacher.lastname)
                    , br [] []
                    , text classroom.name
                    ]

                Empty ->
                    [ p [] [] ]
    in
    div [ class "lesson" ] go


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
