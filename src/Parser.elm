module Parser exposing (..)

import Json.Decode exposing (..)

lessonDecoder : Int -> Int -> Decoder (List (Maybe LessonRecord))
lessonDecoder day l = 
  field "data" <| index day <| at [ "c_" ++ toString l, "cards" ] <| (list (nullable lessonRecordDecoder))

allMessagesInADay : String -> Int -> List (Maybe (List (Maybe LessonRecord)))
allMessagesInADay json day = 
  let 
    getLessons n = Result.toMaybe (decodeString (lessonDecoder day n) json)
  in
    List.map getLessons (List.range 1 9)

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