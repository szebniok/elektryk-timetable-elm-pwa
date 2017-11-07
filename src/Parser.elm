module Parser exposing (..)

import Json.Decode exposing (..)

lessonDecoder : Int -> Int -> Decoder (List (Maybe LessonRecord))
lessonDecoder day l = 
  field "data" <| index day <| at [ "c_" ++ toString l, "cards" ] <| (list (nullable lessonRecordDecoder))

type alias LessonRecord =
  { subject : List String
  , teacher : List String
  , classroom : List String
  }

lessonRecordDecoder : Decoder LessonRecord
lessonRecordDecoder = 
  map3 LessonRecord
    (at ["subjects"] <| list string)
    (at ["teachers"] <| list string)
    (at ["classrooms"] <| list string)