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


getCurrentDate : Cmd Types.Msg
getCurrentDate =
    Task.perform CurrentTime Time.now


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


store : String -> Cmd msg
store str =
    Ports.saveInLocalStorage str


init : Flags -> ( Types.Model, Cmd Types.Msg )
init flags =
    let
        model =
            Model flags.online TimetablePage (Timetable.State.init flags.online) (Substitutions.State.init flags.online)
    in
    case flags.json of
        Just json ->
            ( model, send (FromCache json) )

        Nothing ->
            ( model, send Online )


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
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
            ( { model | timetable = newTimetable, substitutions = newSubstitutions }, Cmd.none )

        SetPage page ->
            ( { model | page = page }, Cmd.none )

        SubstitutionsMsg msg ->
            let
                ( newSubstitutions, cmd ) =
                    Substitutions.State.update msg model.substitutions
            in
            ( { model | substitutions = newSubstitutions }, cmd |> Cmd.map SubstitutionsMsg )

        TimetableMsg msg ->
            let
                ( newTimetable, cmd ) =
                    Timetable.State.update msg model.timetable
            in
            ( { model | timetable = newTimetable }, cmd |> Cmd.map TimetableMsg )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions model =
    Sub.none
