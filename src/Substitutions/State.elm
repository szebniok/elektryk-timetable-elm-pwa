module Substitutions.State exposing (init, update)

import Date
import Ports
import Substitutions.Rest exposing (..)
import Substitutions.Types exposing (..)
import Time


init : Maybe String -> Bool -> Model
init savedTime online =
    let
        timeFromStorage =
            case savedTime of
                Just time ->
                    Just (Time.millisecond * Result.withDefault 0 (String.toFloat time))

                _ ->
                    Nothing
    in
    Model [] 0 timeFromStorage online


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
            ( { model | data = parse data }, store data model.time )

        SubsitutionsFetched (Err _) ->
            ( model, Cmd.none )

        Init (Just data) ->
            case model.savedTime of
                Just oldTime ->
                    let
                        hour =
                            round (Time.inHours oldTime) % 24
                    in
                    ( { model | data = parse data }, Cmd.none )

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
