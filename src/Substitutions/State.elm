module Substitutions.State exposing (init, update)

import Date
import Fetcher exposing (getSubstitutions)
import Substitutions.Rest exposing (..)
import Substitutions.Types exposing (..)
import Time


init : Bool -> Model
init online =
    Model [] 0 online


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
            ( { model | data = parse data }, Cmd.none )

        SubsitutionsFetched (Err _) ->
            ( model, Cmd.none )
