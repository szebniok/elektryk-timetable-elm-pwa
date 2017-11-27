module Timetable.State exposing (init, update)

import Array
import Ports
import Task
import Timetable.Rest exposing (..)
import Timetable.Types exposing (..)


store : String -> Cmd msg
store str =
    Ports.saveInLocalStorage str


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


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

        TouchStart pos ->
            ( { model | touchStart = Just pos }, Cmd.none )

        TouchEnd pos ->
            case model.touchStart of
                Just touchStart ->
                    let
                        diffX =
                            touchStart.clientX - pos.clientX

                        diffY =
                            touchStart.clientY - pos.clientY
                    in
                    if abs diffY > abs diffX then
                        ( model, Cmd.none )
                    else if abs diffX < 60 then
                        ( model, Cmd.none )
                    else if diffX < 0 then
                        { model | touchStart = Nothing }
                            |> update PrevDay
                    else
                        { model | touchStart = Nothing }
                            |> update NextDay

                Nothing ->
                    ( model, Cmd.none )

        NewContent (Ok content) ->
            ( { model | data = parse content }, store content )

        NewContent (Err err) ->
            ( model, Cmd.none )

        FromCache json ->
            ( { model | data = parse json }, Cmd.none )

        Online ->
            ( model, getNewestNumber VersionJson )

        VersionJson (Ok json) ->
            ( model, send (Fetch (globalUpdateParser json)) )

        VersionJson (Err xd) ->
            ( model, Cmd.none )

        Fetch num ->
            ( model, getTimetable NewContent num )

        Update ->
            ( model, send Online )
