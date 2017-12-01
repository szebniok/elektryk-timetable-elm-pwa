module Substitutions.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Substitutions.Types exposing (..)
import Timetable.Types exposing (Classroom, Subject, Teacher)


root : Model -> Html Msg
root model =
    let
        filteredSubstitutions =
            List.filter classPredicate model.data
    in
    div [ class "page" ]
        [ button [ onClick FetchSubstitutions ] [ text "pobierz" ]
        , if List.length filteredSubstitutions == 0 && List.length model.data /= 0 then
            p [] [ text "Brak zastępstw dla twojej klasy" ]
          else
            table []
                (List.map substitution filteredSubstitutions)
        , if model.online then
            p [] [ text "Jestes online" ]
          else
            p [] [ text "Jestes offline" ]
        ]


substitution : Substitution -> Html Msg
substitution sub =
    case sub of
        Substitution period class ( subject, teacher, classroom ) ( oldSubject, oldTeacher, oldClassroom ) ->
            tr []
                [ td [] [ text (toString period) ]
                , td []
                    [ displaySubjectPair subject oldSubject
                    , displayTeacherPair teacher oldTeacher
                    , displayClassroomPair classroom oldClassroom
                    ]
                ]

        _ ->
            text "zredukowane"


classPredicate : Substitution -> Bool
classPredicate sub =
    case sub of
        Substitution _ classes _ _ ->
            List.any (\class -> class.name == "4ct") classes

        NotSupported ->
            False


displaySubjectPair : Subject -> Maybe Subject -> Html msg
displaySubjectPair new old =
    case old of
        Just oldSubject ->
            p []
                [ s [] [ text oldSubject.name ]
                , text (" → " ++ new.name)
                ]

        Nothing ->
            p [] [ text new.name ]


displayTeacherPair : Teacher -> Maybe Teacher -> Html msg
displayTeacherPair new old =
    case old of
        Just oldTeacher ->
            p []
                [ s [] [ text (oldTeacher.firstname ++ " " ++ oldTeacher.lastname) ]
                , text (" → " ++ new.firstname ++ " " ++ new.lastname)
                ]

        Nothing ->
            p [] [ text (new.firstname ++ " " ++ new.lastname) ]


displayClassroomPair : Classroom -> Maybe Classroom -> Html msg
displayClassroomPair new old =
    case old of
        Just oldClassroom ->
            p []
                [ s [] [ text oldClassroom.name ]
                , text (" → " ++ new.name)
                ]

        Nothing ->
            p [] [ text new.name ]
