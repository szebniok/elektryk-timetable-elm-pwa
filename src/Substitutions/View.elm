module Substitutions.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Substitutions.Types exposing (..)


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
                (List.map substitution model.data)
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


classPredicate : Substitution -> Bool
classPredicate sub =
    case sub of
        Substitution _ class _ _ ->
            class.name == "4ct"

        NotSupported ->
            False
