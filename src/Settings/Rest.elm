module Settings.Rest exposing (classListParser, getClasses)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (..)
import Regex exposing (..)
import Substitutions.Types exposing (Class)


headers : List Http.Header
headers =
    [ Http.header "Accept" "*/*"
    , Http.header "Accept-Language" "pl,en-US;q=0.7,en;q=0.3"
    , Http.header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
    , Http.header "X-Requested-With" "XMLHttpRequest"
    ]



{- PARSER -}


classListParser : String -> Dict String Class
classListParser raw =
    let
        classesRegex =
            regex "\"itemdata\":(\\[[^\\]]+)"

        matches =
            find (AtMost 1) classesRegex raw

        firstMatch =
            Maybe.withDefault { match = "", submatches = [ Just "" ], index = 0, number = 0 } <| List.head matches

        result =
            case List.head firstMatch.submatches of
                Just (Just str) ->
                    str ++ "]"

                _ ->
                    ""
    in
    Dict.fromList <| Result.withDefault [] <| decodeString (list makePair) result


makePair : Decoder ( String, Class )
makePair =
    let
        make class id_object =
            let
                id =
                    Maybe.withDefault "" <| Dict.get "id" id_object
            in
            ( id, class )
    in
    map2 make
        (field "text" classRecordDecoder)
        (field "ttb_sel" (dict string))


classRecordDecoder : Decoder Class
classRecordDecoder =
    Json.Decode.map Class
        string



{- REQUEST -}


getClasses : (Result Http.Error String -> msg) -> Cmd msg
getClasses msg =
    Http.send msg classesRequest


classesRequest : Http.Request String
classesRequest =
    let
        params =
            "gadget=MobileEdupage&jscid=gi34476&gsh=6bcf1a53&action=globalReload&&_LJSL=2048"
    in
    Http.request
        { method = "POST"
        , headers = headers
        , url = "https://cors-anywhere.herokuapp.com/https://elektryk.edupage.org/gcall"
        , body = Http.stringBody "text/plain" params
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
