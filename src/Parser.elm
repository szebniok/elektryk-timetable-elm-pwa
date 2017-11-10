module Parser exposing (parse)

import Json.Decode exposing (..)
import Dict exposing (..)

-- TEACHER

type alias Jsdb =
  { teachers : Dict String Teacher
  , subjects : Dict String Subject
  , classroom : Dict String Classroom
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

type alias Lesson = 
  { subject : Subject
  , teacher : Teacher
  , classroom : Classroom
  }


type alias LessonJsonRecord =
  { subject : Int
  , teacher : Int
  , classroom : Int
  }


type alias LessonRecords = 
  List (Maybe (List (Maybe LessonJsonRecord)))


lessonDecoder : Int -> Int -> Decoder (List (Maybe LessonJsonRecord))
lessonDecoder day l = 
  field "data" <| index day <| at [ "c_" ++ toString l, "cards" ] <| (list (nullable lessonJsonRecordDecoder))



allLessonsInADay : String -> Int -> LessonRecords
allLessonsInADay json day = 
  let 
    getLessons n = Result.toMaybe (decodeString (lessonDecoder day n) json)
  in
    List.map getLessons (List.range 1 9)


getAllDays : String -> List LessonRecords
getAllDays json =
  let 
    go n = allLessonsInADay json n
  in
    List.map go (List.range 0 4)




makeLessonJsonRecord : List String -> List String -> List String -> LessonJsonRecord 
makeLessonJsonRecord s t c =
  let 
    parse xs = Result.withDefault 0 (String.toInt (Maybe.withDefault "" (List.head xs)))
  in
    LessonJsonRecord (parse s) (parse t) (parse c)

lessonJsonRecordDecoder : Decoder LessonJsonRecord
lessonJsonRecordDecoder = 
  map3 makeLessonJsonRecord
    (at ["subjects"] <| list string)
    (at ["teachers"] <| list string)
    (at ["classrooms"] <| list string)

-- jsdb

-- type alias LessonRecords = List (Maybe (List (Maybe LessonJsonRecord)))

parse : String -> List (List (Maybe (List (Maybe Lesson))))
parse json =
  let
    rawDays = getAllDays json
    teachers = Result.withDefault Dict.empty (decodeString teachersDecoder json)
    subjects = Result.withDefault Dict.empty (decodeString subjectsDecoder json)
    classrooms = Result.withDefault Dict.empty (decodeString classroomsDecoder json)

    go : (List (Maybe (List (Maybe LessonJsonRecord)))) -> List (Maybe (List (Maybe Lesson)))
    go day = 
      List.map getAllData day

    getAllData day =
      let 
        teacher : String -> Teacher
        teacher x = Maybe.withDefault (Teacher "none" "none" "none") (Dict.get x teachers)
        subject x = Maybe.withDefault (Subject "none") (Dict.get x subjects)
        classroom x = Maybe.withDefault (Classroom "none") (Dict.get x classrooms)


      in 
        Maybe.map (\x -> List.map (\z -> Maybe.map (\y -> Lesson (subject (toString y.subject)) (teacher (toString y.teacher)) (classroom (toString y.classroom))) z) x) day
  in 
    List.map go rawDays
