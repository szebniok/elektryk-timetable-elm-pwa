module Timetable.State exposing (init, update)

import Array
import Timetable.Types exposing (..)


init : Bool -> Model
init online =
    Model 0 Nothing Array.empty online


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrevDay ->
            ( { model | currentDayIndex = max (model.currentDayIndex - 1) 0 }, Cmd.none )

        NextDay ->
            ( { model | currentDayIndex = min (model.currentDayIndex + 1) 4 }, Cmd.none )
