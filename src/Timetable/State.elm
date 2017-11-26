module Timetable.State exposing (init)

import Array
import Timetable.Types exposing (..)


init : Model
init =
    Model 0 Nothing Array.empty
