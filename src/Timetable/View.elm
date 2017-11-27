module Timetable.View exposing (root)

import Array
import Date.Extra.Facts exposing (dayOfWeekFromWeekdayNumber)
import Date.Extra.I18n.I_pl_pl exposing (dayName)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Timetable.Types exposing (..)
import TouchEvents


root : Model -> Html Msg
root model =
    div [ class "page", TouchEvents.onTouchEvent TouchEvents.TouchStart TouchStart, TouchEvents.onTouchEvent TouchEvents.TouchEnd TouchEnd ]
        [ h2 [ class "day-of-week" ]
            [ text (dayName (dayOfWeekFromWeekdayNumber (model.currentDayIndex + 1))) ]
        , displayTable model.currentDayIndex model.data

        {- , if model.online then
             button [ onClick Update ] [ text "Pobierz nowa zawartosc" ]
           else
             p [] [ text "Jestes offline" ]
        -}
        ]


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
