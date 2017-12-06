module Timetable.Rest exposing (..)

import Array
import Dict exposing (Dict)
import Http
import Json.Decode exposing (..)
import List
import Regex exposing (HowMany(AtMost), find, regex)
import Timetable.Types exposing (..)


-- HTTP


headers : List Http.Header
headers =
    [ Http.header "Accept" "*/*"
    , Http.header "Accept-Language" "pl,en-US;q=0.7,en;q=0.3"
    , Http.header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
    , Http.header "X-Requested-With" "XMLHttpRequest"
    ]


getTimetable : (Result Http.Error String -> msg) -> Int -> String -> Cmd msg
getTimetable msg num classId =
    Http.send msg (timetableRequest num classId)


globalUpdateRequest : Http.Request String
globalUpdateRequest =
    let
        params =
            "gadget=MobileEdupage&jscid=gi34476&gsh=6bcf1a53&action=globalReload&&_LJSL=2048"
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "https://cors-anywhere.herokuapp.com/https://elektryk.edupage.org/gcall"
        , body = Http.stringBody "text/plain" params
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


getNewestNumber : (Result Http.Error String -> msg) -> Cmd msg
getNewestNumber msg =
    Http.send msg globalUpdateRequest



-- TIMETABLE


timetableRequest : Int -> String -> Http.Request String
timetableRequest num classId =
    let
        params =
            "gadget=MobileTimetableBrowser&jscid=gi40229&gsh=6bcf1a53&action=reload&num=" ++ toString num ++ "&oblast=trieda&id=" ++ classId ++ "&_LJSL=2048"
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "https://cors-anywhere.herokuapp.com/https://elektryk.edupage.org/gcall"
        , body = Http.stringBody "text/plain" params
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }



-- JSON


parse : String -> Timetable
parse json =
    let
        jsdbRegex =
            regex "obj\\.loadTimetable\\(\"\\d+\",\"trieda\",\"[-]?\\d+\",([\\s\\S]+)\\);gi\\d+\\.obj\\.renderTT"

        matches =
            find (AtMost 1) jsdbRegex json

        firstMatch =
            Maybe.withDefault { match = "", submatches = [ Just "" ], index = 0, number = 0 } <| List.head matches

        result =
            case List.head firstMatch.submatches of
                Just (Just str) ->
                    str

                _ ->
                    ""

        jsdbData =
            makeJsdb result
    in
    getAllDays jsdbData result


globalUpdateParser : String -> Int
globalUpdateParser json =
    let
        numRegex =
            regex "jsc_timetable.obj.loadSettings\\({\"num\":(\\d+)}\\);"

        matches =
            find (AtMost 1) numRegex json

        firstMatch =
            Maybe.withDefault { match = "", submatches = [ Just "" ], index = 0, number = 0 } <| List.head matches

        result =
            case List.head firstMatch.submatches of
                Just (Just str) ->
                    String.toInt str

                _ ->
                    Result.Ok 0
    in
    Result.withDefault 0 result



-- TEACHER


type alias Jsdb =
    { teachers : Dict String Teacher
    , subjects : Dict String Subject
    , classrooms : Dict String Classroom
    }


makeJsdb : String -> Jsdb
makeJsdb json =
    let
        teachers =
            Result.withDefault Dict.empty (decodeString teachersDecoder json)

        subjects =
            Result.withDefault Dict.empty (decodeString subjectsDecoder json)

        classrooms =
            Result.withDefault Dict.empty (decodeString classroomsDecoder json)
    in
    Jsdb teachers subjects classrooms


teacherRecordDecoder : Decoder Teacher
teacherRecordDecoder =
    map3 Teacher
        (field "firstname" string)
        (field "lastname" string)
        (field "short" string)


teachersDecoder : Decoder (Dict String Teacher)
teachersDecoder =
    field "jsdb" <| field "teachers" <| dict teacherRecordDecoder



-- SUBJECT


subjectRecordDecoder : Decoder Subject
subjectRecordDecoder =
    Json.Decode.map Subject (field "name" string)


subjectsDecoder : Decoder (Dict String Subject)
subjectsDecoder =
    field "jsdb" <| field "subjects" <| dict subjectRecordDecoder



-- CLASSROOM


classroomRecordDecoder : Decoder Classroom
classroomRecordDecoder =
    Json.Decode.map Classroom (field "name" string)


classroomsDecoder : Decoder (Dict String Classroom)
classroomsDecoder =
    field "jsdb" <| field "classrooms" <| dict classroomRecordDecoder



-- LESSON


lessonsDecoder : Jsdb -> Int -> Int -> Decoder (List Lesson)
lessonsDecoder jsdb day lesson =
    field "data" <| index day <| at [ "c_" ++ toString lesson, "cards" ] <| list (oneOf [ null Empty, lessonDecoder jsdb ])


allLessonsInADay : Jsdb -> String -> Int -> TimetableRow
allLessonsInADay jsdb json day =
    let
        getLessons lesson =
            case decodeString (lessonsDecoder jsdb day lesson) json of
                Err _ ->
                    NoLessons

                Ok cell ->
                    Lessons cell
    in
    List.map getLessons (List.range 1 9)


getAllDays : Jsdb -> String -> Timetable
getAllDays jsdb json =
    let
        go n =
            allLessonsInADay jsdb json n
    in
    Array.fromList (List.map go (List.range 0 4))


lessonDecoder : Jsdb -> Decoder Lesson
lessonDecoder jsdb =
    map3 (makeLessonFromJsonTuple jsdb)
        (field "subjects" <| list string)
        (field "teachers" <| list string)
        (field "classrooms" <| list string)


makeLessonFromJsonTuple : Jsdb -> List String -> List String -> List String -> Lesson
makeLessonFromJsonTuple jsdb subjects teachers classrooms =
    let
        getKey xs =
            Maybe.withDefault "0" (List.head xs)

        subjectKey =
            getKey subjects

        teacherKey =
            getKey teachers

        classroomKey =
            getKey classrooms

        subject =
            Maybe.withDefault (Subject "none") (Dict.get subjectKey jsdb.subjects)

        teacher =
            Maybe.withDefault (Teacher "none" "none" "none") (Dict.get teacherKey jsdb.teachers)

        classroom =
            Maybe.withDefault (Classroom "none") (Dict.get classroomKey jsdb.classrooms)
    in
    Lesson (LessonData subject teacher classroom)
