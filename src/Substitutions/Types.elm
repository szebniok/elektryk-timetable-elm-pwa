module Substitutions.Types exposing (..)

import Timetable.Types exposing (..)


type alias Model =
    { data : List Substitution }


type Substitution
    = Substitution Period Class ( Subject, Teacher, Classroom ) ( Maybe Subject, Maybe Teacher, Maybe Classroom )
    | NotSupported -- fallback type, for example supervision changes are not supported


type alias Period =
    Int


type alias Class =
    { name : String }
