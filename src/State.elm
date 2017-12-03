module State exposing (init, subscriptions, update)

import Date
import Date.Extra.Core
import Navigation exposing (..)
import Ports
import Substitutions.State
import Substitutions.Types exposing (Msg(Init))
import Task
import Time
import Timetable.State
import Timetable.Types exposing (Msg(FromCache, Online))
import Types exposing (..)


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


getCurrentDate : Cmd Types.Msg
getCurrentDate =
    Task.perform CurrentTime Time.now


init : Flags -> Location -> ( Types.Model, Cmd Types.Msg )
init flags location =
    let
        model =
            Model flags.online (parseLocation location) (Timetable.State.init flags.online) (Substitutions.State.init flags.savedTime flags.online) flags.substitutions
    in
    case flags.timetable of
        Just timetableJson ->
            ( model, Cmd.batch [ getCurrentDate, send (TimetableMsg (FromCache timetableJson)) ] )

        Nothing ->
            ( model, Cmd.batch [ getCurrentDate, send (TimetableMsg Online) ] )


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update msg model =
    case msg of
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
            { model | timetable = newTimetable, substitutions = newSubstitutions }
                |> update (SubstitutionsMsg << Init <| model.substitutionsFromStorage)

        SetPage page ->
            ( { model | page = page }, Cmd.batch [ Navigation.newUrl (reversePage page), Ports.trackPageview (reversePage page) ] )

        UrlChange location ->
            ( { model | page = parseLocation location }, Cmd.none )

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
