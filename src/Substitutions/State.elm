module Substitutions.State exposing (init, update)

import Date exposing (Date)
import Ports
import Substitutions.Rest exposing (..)
import Substitutions.Types exposing (..)
import Time
import Timetable.Types exposing (Class)


init : Maybe String -> Bool -> Class -> Model
init savedTime online class =
    let
        timeFromStorage =
            case savedTime of
                Just time ->
                    Just (Time.millisecond * Result.withDefault 0 (String.toFloat time))

                _ ->
                    Nothing
    in
    Model [] 0 timeFromStorage online class


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            let
                parsedData =
                    parse data
            in
            if List.length parsedData /= 0 then
                ( { model | data = parsedData, savedTime = Just model.time }, store data model.time )
            else
                ( model, Cmd.none )

        SubsitutionsFetched (Err _) ->
            ( model, Cmd.none )

        Init (Just data) ->
            case model.savedTime of
                Just oldTime ->
                    let
                        hour =
                            round (Time.inHours model.time) % 24

                        oldHour =
                            round (Time.inHours oldTime) % 24

                        oldDate =
                            Date.fromTime oldTime

                        today =
                            Date.fromTime model.time
                    in
                    if isSameDay today oldDate then
                        if hour > 15 && oldHour < 15 then
                            update FetchSubstitutions model
                        else
                            ( { model | data = parse data }, Cmd.none )
                    else if Time.inHours (model.time - oldTime) < 24 && oldHour > 15 && hour < 15 then
                        ( { model | data = parse data }, Cmd.none )
                    else
                        update FetchSubstitutions model

                Nothing ->
                    update FetchSubstitutions model

        Init Nothing ->
            update FetchSubstitutions model


store : String -> Time.Time -> Cmd msg
store data time =
    Cmd.batch
        [ Ports.saveInLocalStorage ( "substitutions", data )
        , Ports.saveInLocalStorage ( "substitutions-time", toString time )
        ]


isSameDay : Date -> Date -> Bool
isSameDay a b =
    Date.year a == Date.year b && Date.month a == Date.month b && Date.day a == Date.day b
