module Types exposing (..)

import Http
import Substitutions.Types
import Time
import Timetable.Types


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
    | CurrentTime Time.Time
    | SetPage Page
    | SubstitutionsMsg Substitutions.Types.Msg
    | TimetableMsg Timetable.Types.Msg
