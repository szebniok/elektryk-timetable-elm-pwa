module Substitutions.Rest exposing (parse)

import Dict exposing (Dict)
import Json.Decode exposing (..)
import List
import Parser exposing (..)
import Regex exposing (HowMany(AtMost), find, regex)
import Substitutions.Types exposing (..)
import Timetable.Types exposing (..)


parse : String -> List Substitution
parse raw =
    let
        jsdb =
            substitutionDatabaseParser raw

        jsonRegex =
            regex "obj\\.reloadRows\\(\"\\d{4}-\\d{2}-\\d{2}\",([^)]+)\\)"

        matches =
            find (AtMost 1) jsonRegex raw

        json =
            List.head matches
                |> Maybe.andThen (\x -> List.head x.submatches)
                |> Maybe.andThen (\x -> x)
                |> Maybe.withDefault ""
    in
    -- we are filtering out not supported types
    List.filter (\x -> x /= NotSupported) <| Result.withDefault [] (decodeString (list <| substitutionParserDecoder jsdb) json)


type alias SubstitutionJsdb =
    { teachers : Dict String Teacher
    , subjects : Dict String Subject
    , classrooms : Dict String Classroom
    , classes : Dict String Class
    }


classRecordDecoder : Decoder Class
classRecordDecoder =
    Json.Decode.map Class
        (field "name" string)


substitutionDatabaseTeachersDecoder : Decoder (Dict String Teacher)
substitutionDatabaseTeachersDecoder =
    field "teachers" <| dict teacherRecordDecoder


substitutionDatabaseLessonsDecoder : Decoder (Dict String Subject)
substitutionDatabaseLessonsDecoder =
    field "subjects" <| dict subjectRecordDecoder


substitutionDatabaseClassroomsDecoder : Decoder (Dict String Classroom)
substitutionDatabaseClassroomsDecoder =
    field "classrooms" <| dict classroomRecordDecoder


substitutionDatabaseClassesDecoder : Decoder (Dict String Class)
substitutionDatabaseClassesDecoder =
    field "classes" <| dict classRecordDecoder


substitutionDatabaseParser : String -> SubstitutionJsdb
substitutionDatabaseParser raw =
    let
        dbRegex =
            regex "obj\\.db_fill\\(([^)]+)\\)"

        matches =
            find (AtMost 1) dbRegex raw

        json =
            List.head matches
                |> Maybe.andThen (\x -> List.head x.submatches)
                |> Maybe.andThen (\x -> x)
                |> Maybe.withDefault ""

        teachers =
            Result.withDefault Dict.empty (decodeString substitutionDatabaseTeachersDecoder json)

        subjects =
            Result.withDefault Dict.empty (decodeString substitutionDatabaseLessonsDecoder json)

        classrooms =
            Result.withDefault Dict.empty (decodeString substitutionDatabaseClassroomsDecoder json)

        classes =
            Result.withDefault Dict.empty (decodeString substitutionDatabaseClassesDecoder json)
    in
    SubstitutionJsdb teachers subjects classrooms classes


type alias Change =
    { column : String
    , oldId : String
    , newId : String
    }


changeDecoder : Decoder Change
changeDecoder =
    map3 Change
        (field "column" string)
        (field "old" string)
        (field "new" string)


substitutionParserDecoder : SubstitutionJsdb -> Decoder Substitution
substitutionParserDecoder jsdb =
    let
        make periodString classIds changes subjectIdStr teacherIds classroomIds =
            let
                listToValue : List String -> Dict String a -> Maybe a
                listToValue key dict =
                    List.head key
                        |> Maybe.andThen (\x -> Dict.get x dict)

                oldTeacher =
                    case List.filter (\change -> change.column == "teacherids") changes of
                        { column, oldId, newId } :: _ ->
                            Dict.get oldId jsdb.teachers

                        _ ->
                            Nothing

                oldSubject =
                    case List.filter (\change -> change.column == "subjectids") changes of
                        { column, oldId, newId } :: _ ->
                            Dict.get oldId jsdb.subjects

                        [] ->
                            Nothing

                oldClassroom =
                    case List.filter (\change -> change.column == "classroomids") changes of
                        { column, oldId, newId } :: _ ->
                            Dict.get oldId jsdb.classrooms

                        [] ->
                            Nothing

                period =
                    Result.withDefault 0 (String.toInt periodString)

                class =
                    listToValue classIds jsdb.classes |> Maybe.withDefault (Class "none")

                subject =
                    Dict.get subjectIdStr jsdb.subjects |> Maybe.withDefault (Subject "none")

                teacher =
                    listToValue teacherIds jsdb.teachers |> Maybe.withDefault (Teacher "none" "none" "none")

                classroom =
                    listToValue classroomIds jsdb.classrooms |> Maybe.withDefault (Classroom "none")
            in
            Substitution period class ( subject, teacher, classroom ) ( oldSubject, oldTeacher, oldClassroom )
    in
    oneOf
        [ map6 make
            (field "period" (oneOf [ string, Json.Decode.map toString int ]))
            (field "classids" <| list string)
            (field "changes" <| list changeDecoder)
            (field "subjectid" string)
            (field "teacherids" <| list string)
            (field "classroomids" <| list string)
        , succeed NotSupported
        ]
