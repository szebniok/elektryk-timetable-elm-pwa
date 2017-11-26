module State exposing (init, subscriptions, update)

import Date
import Date.Extra.Core
import Ports
import Substitutions.State
import Task
import Time
import Timetable.Rest exposing (..)
import Timetable.State
import Types exposing (..)


getCurrentDate : Cmd Msg
getCurrentDate =
    Task.perform CurrentTime Time.now


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


store : String -> Cmd msg
store str =
    Ports.saveInLocalStorage str


init : Flags -> ( Types.Model, Cmd Msg )
init flags =
    let
        model =
            Model flags.online TimetablePage 0 Timetable.State.init (Substitutions.State.init flags.online)
    in
    case flags.json of
        Just json ->
            ( model, send (FromCache json) )

        Nothing ->
            ( model, send Online )


update : Msg -> Types.Model -> ( Types.Model, Cmd Msg )
update msg model =
    case msg of
        NewContent (Ok content) ->
            -- returned JSON is embedded in a call to JS function, so we strip out unnecessary characters from both sides
            let
                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | data = parse content }
            in
            ( { model | timetable = newTimetable }, Cmd.batch [ store content, getCurrentDate ] )

        NewContent (Err err) ->
            ( model, Cmd.none )

        FromCache json ->
            let
                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | data = parse json }
            in
            ( { model | timetable = newTimetable }, getCurrentDate )

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

        PrevDay ->
            let
                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | currentDayIndex = max (oldTimetable.currentDayIndex - 1) 0 }
            in
            ( { model | timetable = newTimetable }, Cmd.none )

        NextDay ->
            let
                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | currentDayIndex = min (oldTimetable.currentDayIndex + 1) 4 }
            in
            ( { model | timetable = newTimetable }, Cmd.none )

        CurrentTime time ->
            let
                hour =
                    round (Time.inHours time) % 24

                date =
                    Date.fromTime time

                day =
                    Date.dayOfWeek date

                dayToDisplay =
                    case day of
                        Date.Sat ->
                            Date.Mon

                        Date.Sun ->
                            Date.Mon

                        day ->
                            if hour > 15 then
                                if day == Date.Fri then
                                    Date.Mon
                                else
                                    Date.Extra.Core.nextDay day
                            else
                                day

                dayIndex =
                    Date.Extra.Core.isoDayOfWeek dayToDisplay - 1

                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | currentDayIndex = dayIndex }

                oldSubstitutions =
                    model.substitutions

                newSubstitutions =
                    { oldSubstitutions | time = time }
            in
            ( { model | timetable = newTimetable, substitutions = newSubstitutions, time = time }, Cmd.none )

        TouchStart pos ->
            let
                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | touchStart = Just pos }
            in
            ( { model | timetable = newTimetable }, Cmd.none )

        TouchEnd pos ->
            case model.timetable.touchStart of
                Just touchStart ->
                    let
                        diffX =
                            touchStart.clientX - pos.clientX

                        diffY =
                            touchStart.clientY - pos.clientY

                        oldTimetable =
                            model.timetable

                        timetableWithoutTouchStart =
                            { oldTimetable | touchStart = Nothing }
                    in
                    if abs diffY > abs diffX then
                        ( model, Cmd.none )
                    else if abs diffX < 60 then
                        ( model, Cmd.none )
                    else if diffX < 0 then
                        { model | timetable = timetableWithoutTouchStart }
                            |> update PrevDay
                    else
                        { model | timetable = timetableWithoutTouchStart }
                            |> update NextDay

                Nothing ->
                    ( model, Cmd.none )

        SetPage page ->
            ( { model | page = page }, Cmd.none )

        SubstitutionsMsg msg ->
            let
                ( newSubstitutions, cmd ) =
                    Substitutions.State.update msg model.substitutions
            in
            ( { model | substitutions = newSubstitutions }, Cmd.none )


subscriptions : Types.Model -> Sub Msg
subscriptions model =
    Sub.none
