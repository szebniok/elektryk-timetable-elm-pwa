module Parser exposing (..)

import Json.Decode exposing (..)

lessonDecoder : Int -> Int -> Decoder (List (Maybe LessonRecord))
lessonDecoder day l = 
  field "data" <| index day <| at [ "c_" ++ toString l, "cards" ] <| (list (nullable lessonRecordDecoder))

type alias Lessons = List (Maybe (List (Maybe LessonRecord)))

allLessonsInADay : String -> Int -> Lessons
allLessonsInADay json day = 
  let 
    getLessons n = Result.toMaybe (decodeString (lessonDecoder day n) json)
  in
    List.map getLessons (List.range 1 9)


getAllDays : String -> List Lessons
getAllDays json =
  let 
    go n = allLessonsInADay json n
  in
    List.map go (List.range 0 4)


type alias LessonRecord =
  { subject : Int
  , teacher : Int
  , classroom : Int
  }

makeLessonRecord : List String -> List String -> List String -> LessonRecord 
makeLessonRecord s t c =
  let 
    parse xs = Result.withDefault 0 (String.toInt (Maybe.withDefault "" (List.head xs)))
  in
    LessonRecord (parse s) (parse t) (parse c)

lessonRecordDecoder : Decoder LessonRecord
lessonRecordDecoder = 
  map3 makeLessonRecord
    (at ["subjects"] <| list string)
    (at ["teachers"] <| list string)
    (at ["classrooms"] <| list string)

-- jsdb
type alias TeacherJsonRecord =
  { firstname: String
  , lastname: String
  , short: String
  }

teacherJsonRecordDecoder : Decoder TeacherJsonRecord
teacherJsonRecordDecoder =
  map3 TeacherJsonRecord
    (at ["firstname"] string)
    (at ["lastname"] string)
    (at ["short"] string)

teachersDecoder = 
  field "jsdb" <| field "teachers" <| dict teacherJsonRecordDecoder
