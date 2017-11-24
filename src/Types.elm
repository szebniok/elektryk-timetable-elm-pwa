module Types exposing (..)

import Array
import Http
import Time
import TouchEvents


type Page
    = TimetablePage
    | SubstitutionsPage


type alias Flags =
    { online : Bool
    , json : Maybe String
    }


type alias Model =
    { online : Bool
    , data : Timetable
    , currentDayIndex : Int
    , touchStart : Maybe TouchEvents.Touch
    , page : Page
    , substitutions : List Substitution
    , time : Time.Time
    }


type Msg
    = NewContent (Result Http.Error String)
    | FromCache String
    | Online
    | VersionJson (Result Http.Error String)
    | Fetch Int
    | Update
    | NextDay
    | PrevDay
    | CurrentTime Time.Time
    | TouchStart TouchEvents.Touch
    | TouchEnd TouchEvents.Touch
    | SetPage Page
    | FetchSubstitutions
    | SubsitutionsFetched (Result Http.Error String)


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



-- LESSON


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


type Substitution
    = Substitution Period Class ( Subject, Teacher, Classroom ) ( Maybe Subject, Maybe Teacher, Maybe Classroom )
    | NotSupported -- fallback type, for example supervision changes are not supported


type alias Period =
    Int


type alias Class =
    { name : String }
