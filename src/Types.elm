module Types exposing (..)

import Http
import Navigation exposing (Location)
import Substitutions.Types
import Time
import Timetable.Types exposing (Class)
import UrlParser as Url


type Page
    = TimetablePage
    | SubstitutionsPage
    | SettingsPage
    | NotFoundPage


type alias Flags =
    { online : Bool
    , timetable : Maybe String
    , substitutions : Maybe String
    , savedTime : Maybe String
    , class : Maybe String
    , classesList : Maybe String
    }


type alias Model =
    { online : Bool
    , page : Page
    , timetable : Timetable.Types.Model
    , substitutions : Substitutions.Types.Model
    , substitutionsFromStorage : Maybe String
    , classes : List Class
    , activeClass : Class
    }


type Msg
    = CurrentTime Time.Time
    | SetPage Page
    | UrlChange Navigation.Location
    | SubstitutionsMsg Substitutions.Types.Msg
    | TimetableMsg Timetable.Types.Msg
    | GetClasses (Result Http.Error String)
    | DownloadClasses
    | SetClass String



-- LOCATION


reversePage : Page -> String
reversePage page =
    case page of
        TimetablePage ->
            "/"

        SubstitutionsPage ->
            "/substitutions"

        SettingsPage ->
            "/settings"

        NotFoundPage ->
            ""


routeParser : Url.Parser (Page -> a) a
routeParser =
    Url.oneOf
        [ Url.map TimetablePage Url.top
        , Url.map SubstitutionsPage (Url.s "substitutions")
        , Url.map SettingsPage (Url.s "settings")
        ]


parseLocation : Location -> Page
parseLocation location =
    location
        |> Url.parsePath routeParser
        |> Maybe.withDefault NotFoundPage
