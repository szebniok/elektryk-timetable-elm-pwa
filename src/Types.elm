module Types exposing (..)

import Http
import Substitutions.Types
import Time
import Timetable.Types
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
    , time : Time.Time
    , timetable : Timetable.Types.Model
    , substitutions : Substitutions.Types.Model
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
