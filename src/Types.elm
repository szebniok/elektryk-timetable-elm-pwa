module Types exposing (..)

import Http
import Time
import Timetable.Types exposing (..)
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
    , page : Page
    , substitutions : List Substitution
    , time : Time.Time
    , timetable : Timetable.Types.Model
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



-- LESSON


type Substitution
    = Substitution Period Class ( Subject, Teacher, Classroom ) ( Maybe Subject, Maybe Teacher, Maybe Classroom )
    | NotSupported -- fallback type, for example supervision changes are not supported


type alias Period =
    Int


type alias Class =
    { name : String }
