module Timetable.State exposing (init)

import Array
import Timetable.Types exposing (..)


init : Bool -> Model
init online =
    Model 0 Nothing Array.empty online
