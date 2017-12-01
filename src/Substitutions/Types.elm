module Substitutions.Types exposing (..)

import Http
import Time exposing (Time)
import Timetable.Types exposing (..)


type alias Model =
    { data : List Substitution
    , time : Time
    , online : Bool
    }


type Msg
    = FetchSubstitutions
    | SubsitutionsFetched (Result Http.Error String)
    | Init (Maybe String)


type Substitution
    = Substitution Period (List Class) ( Subject, Teacher, Classroom ) ( Maybe Subject, Maybe Teacher, Maybe Classroom )
    | NotSupported -- fallback type, for example supervision changes are not supported


type alias Period =
    Int


type alias Class =
    { name : String }
