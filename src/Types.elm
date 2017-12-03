module Types exposing (..)

import Navigation exposing (Location)
import Substitutions.Types
import Time
import Timetable.Types
import UrlParser as Url


type Page
    = TimetablePage
    | SubstitutionsPage
    | NotFoundPage


type alias Flags =
    { online : Bool
    , timetable : Maybe String
    , substitutions : Maybe String
    , savedTime : Maybe String
    }


type alias Model =
    { online : Bool
    , page : Page
    , timetable : Timetable.Types.Model
    , substitutions : Substitutions.Types.Model
    , substitutionsFromStorage : Maybe String
    }


type Msg
    = CurrentTime Time.Time
    | SetPage Page
    | UrlChange Navigation.Location
    | SubstitutionsMsg Substitutions.Types.Msg
    | TimetableMsg Timetable.Types.Msg



-- LOCATION


reversePage : Page -> String
reversePage page =
    case page of
        TimetablePage ->
            "/"

        SubstitutionsPage ->
            "/substitutions"

        NotFoundPage ->
            ""


routeParser : Url.Parser (Page -> a) a
routeParser =
    Url.oneOf
        [ Url.map TimetablePage Url.top
        , Url.map SubstitutionsPage (Url.s "substitutions")
        ]


parseLocation : Location -> Page
parseLocation location =
    location
        |> Url.parsePath routeParser
        |> Maybe.withDefault NotFoundPage
