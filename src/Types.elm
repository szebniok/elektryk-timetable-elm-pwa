module Types exposing (..)

import Navigation
import Substitutions.Types
import Time
import Timetable.Types


type Page
    = TimetablePage
    | SubstitutionsPage
    | NotFoundPage


type alias Flags =
    { online : Bool
    , json : Maybe String
    }


type alias Model =
    { online : Bool
    , page : Page
    , history : List Navigation.Location
    , timetable : Timetable.Types.Model
    , substitutions : Substitutions.Types.Model
    }


type Msg
    = CurrentTime Time.Time
    | SetPage Page
    | UrlChange Navigation.Location
    | SubstitutionsMsg Substitutions.Types.Msg
    | TimetableMsg Timetable.Types.Msg
