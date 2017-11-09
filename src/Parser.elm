module Parser exposing (..)

import Json.Decode exposing (..)

lessonDecoder : Int -> Int -> Decoder (List (Maybe LessonJsonRecord))
lessonDecoder day l = 
  field "data" <| index day <| at [ "c_" ++ toString l, "cards" ] <| (list (nullable lessonJsonRecordDecoder))

type alias LessonRecords = List (Maybe (List (Maybe LessonJsonRecord)))

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


type alias LessonJsonRecord =
  { subject : Int
  , teacher : Int
  , classroom : Int
  }

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
type alias TeacherRecord =
  { firstname: String
  , lastname: String
  , short: String
  }

teacherRecordDecoder : Decoder TeacherRecord
teacherRecordDecoder =
  map3 TeacherRecord
    (at ["firstname"] string)
    (at ["lastname"] string)
    (at ["short"] string)

teachersDecoder = 
  field "jsdb" <| field "teachers" <| dict teacherRecordDecoder


type alias SubjectRecord =
  { name : String }

subjectRecordDecoder : Decoder SubjectRecord
subjectRecordDecoder =
  map SubjectRecord (at ["name"] string)

subjectsDecoder = 
  field "jsdb" <| field "subjects" <| dict subjectRecordDecoder



type alias ClassroomRecord = 
  { name : String }

classroomRecordDecoder = 
  map ClassroomRecord (at ["name"] string)

classroomsDecoder = 
  field "jsdb" <| field "classrooms" <| dict classroomRecordDecoder


