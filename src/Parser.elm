module Parser exposing (parse, Timetable, TimetableRow, TimetableCell(Lessons, NoLessons), Lesson(Lesson, Empty), LessonData)

import Json.Decode exposing (..)
import Dict exposing (..)
import Regex exposing (HowMany(AtMost), find, regex)

-- VERSION NUMBER

globalUpdateParser : String -> Int
globalUpdateParser json =
  let 
    numRegex = regex "jsc_timetable.obj.loadSettings\\({\"num\":(\\d+)}\\);"
    matches = find (AtMost 1) numRegex json
    firstMatch = Maybe.withDefault {match = "", submatches = [Just ""], index = 0, number = 0} <| List.head matches
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
    teachers = Result.withDefault Dict.empty (decodeString teachersDecoder json)
    subjects = Result.withDefault Dict.empty (decodeString subjectsDecoder json)
    classrooms = Result.withDefault Dict.empty (decodeString classroomsDecoder json)
  in
    Jsdb teachers subjects classrooms


type alias Teacher =
  { firstname: String
  , lastname: String
  , short: String
  }


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

type alias Subject =
  { name : String }


subjectRecordDecoder : Decoder Subject
subjectRecordDecoder =
  Json.Decode.map Subject (field "name" string)


subjectsDecoder : Decoder (Dict String Subject)
subjectsDecoder = 
  field "jsdb" <| field "subjects" <| dict subjectRecordDecoder



-- CLASSROOM

type alias Classroom = 
  { name : String }


classroomRecordDecoder : Decoder Classroom
classroomRecordDecoder = 
  Json.Decode.map Classroom (field "name" string)


classroomsDecoder : Decoder (Dict String Classroom)
classroomsDecoder = 
  field "jsdb" <| field "classrooms" <| dict classroomRecordDecoder



-- LESSON

type alias Timetable
  = List TimetableRow


type alias TimetableRow
  = List TimetableCell


type TimetableCell 
  = Lessons (List Lesson)
  | NoLessons


type alias LessonData = 
  { subject : Subject
  , teacher : Teacher
  , classroom : Classroom
  }


type Lesson
  = Lesson LessonData
  | Empty


parse : String -> Timetable
parse json =
  let 
    jsdbData = makeJsdb json
  in
    getAllDays jsdbData json


lessonsDecoder : Jsdb -> Int -> Int -> Decoder (List Lesson)
lessonsDecoder jsdb day lesson = 
  field "data" <| index day <| at [ "c_" ++ toString lesson, "cards" ] <| (list (oneOf [ null Empty, (lessonDecoder jsdb)]))


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
    go n = allLessonsInADay jsdb json n
  in
    List.map go (List.range 0 4)


lessonDecoder : Jsdb -> Decoder Lesson
lessonDecoder jsdb = 
  map3 (makeLessonFromJsonTuple jsdb)
    (field "subjects" <| list string)
    (field "teachers" <| list string)
    (field "classrooms" <| list string)

makeLessonFromJsonTuple : Jsdb -> List String -> List String -> List String -> Lesson
makeLessonFromJsonTuple jsdb subjects teachers classrooms =
  let 
    getKey xs = Maybe.withDefault "0" (List.head xs)

    subjectKey = getKey subjects
    teacherKey = getKey teachers
    classroomKey = getKey classrooms

    subject = Maybe.withDefault (Subject "none") (Dict.get subjectKey jsdb.subjects)
    teacher = Maybe.withDefault (Teacher "none" "none" "none") (Dict.get teacherKey jsdb.teachers)
    classroom = Maybe.withDefault (Classroom "none") (Dict.get classroomKey jsdb.classrooms)
  in
    Lesson (LessonData subject teacher classroom)