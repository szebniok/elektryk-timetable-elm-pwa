module Timetable.Types exposing (..)

import Array
import TouchEvents


type alias Model =
    { currentDayIndex : Int
    , touchStart : Maybe TouchEvents.Touch
    , data : Timetable
    , online : Bool
    }


type Msg
    = NextDay
    | PrevDay
    | TouchStart TouchEvents.Touch
    | TouchEnd TouchEvents.Touch


type alias Timetable =
    Array.Array TimetableRow


type alias TimetableRow =
    List TimetableCell


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


type alias Teacher =
    { firstname : String
    , lastname : String
    , short : String
    }



-- SUBJECT


type alias Subject =
    { name : String }



-- CLASSROOM


type alias Classroom =
    { name : String }
