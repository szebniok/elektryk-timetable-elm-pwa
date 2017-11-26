module State exposing (init, subscriptions, update)

import Date
import Date.Extra.Core
import Fetcher exposing (getNewestNumber, getSubstitutions, getTimetable)
import Parser exposing (..)
import Ports
import Task
import Time
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
    case flags.json of
        Just json ->
            ( Types.Model flags.online TimetablePage [] 0 Timetable.State.init, send (FromCache json) )

        Nothing ->
            ( Types.Model flags.online TimetablePage [] 0 Timetable.State.init, send Online )


update : Msg -> Types.Model -> ( Types.Model, Cmd Msg )
update msg model =
    case msg of
        NewContent (Ok content) ->
            -- returned JSON is embedded in a call to JS function, so we strip out unnecessary characters from both sides
            let
                newContent =
                    content |> String.dropLeft 103 |> String.dropRight 43

                parsedContent =
                    parse newContent

                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | data = parsedContent }
            in
            ( { model | timetable = newTimetable }, Cmd.batch [ store content, getCurrentDate ] )

        NewContent (Err err) ->
            ( model, Cmd.none )

        FromCache json ->
            let
                newContent =
                    json |> String.dropLeft 103 |> String.dropRight 43

                parsedContent =
                    parse newContent

                oldTimetable =
                    model.timetable

                newTimetable =
                    { oldTimetable | data = parsedContent }
            in
            ( { model | timetable = newTimetable }, getCurrentDate )

        Online ->
            ( model, getNewestNumber VersionJson )

        VersionJson (Ok json) ->
            ( model, send (Fetch (Parser.globalUpdateParser json)) )

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
                    { oldTimetable | currentDayIndex = max (oldTimetable.currentDayIndex + 1) 4 }
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
            in
            ( { model | timetable = newTimetable, time = time }, Cmd.none )

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

        FetchSubstitutions ->
            let
                hour =
                    round (Time.inHours model.time) % 24

                offset =
                    if hour > 15 then
                        Time.hour * 24
                    else
                        0

                date =
                    Date.fromTime (model.time + offset)
            in
            ( model, getSubstitutions SubsitutionsFetched date )

        SubsitutionsFetched (Ok data) ->
            ( { model | substitutions = substitutionsParser data }, Cmd.none )

        SubsitutionsFetched (Err _) ->
            ( model, Cmd.none )


subscriptions : Types.Model -> Sub Msg
subscriptions model =
    Sub.none
