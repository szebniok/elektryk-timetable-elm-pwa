module Types exposing (..)

import Http
import Parser exposing (Lesson(Empty, Lesson), Substitution(Substitution), Timetable, TimetableCell(Lessons, NoLessons), TimetableRow, parse, substitutionsParser)
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
