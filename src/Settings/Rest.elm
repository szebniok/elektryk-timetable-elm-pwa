module Settings.Rest exposing (classListParser, getClasses)

import Http
import Json.Decode exposing (..)
import Regex exposing (..)
import Timetable.Types exposing (Class)


headers : List Http.Header
headers =
    [ Http.header "Accept" "*/*"
    , Http.header "Accept-Language" "pl,en-US;q=0.7,en;q=0.3"
    , Http.header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
    , Http.header "X-Requested-With" "XMLHttpRequest"
    ]



{- PARSER -}


classListParser : String -> List Class
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
    Result.withDefault [] <| decodeString (list classRecordDecoder) result


classRecordDecoder : Decoder Class
classRecordDecoder =
    map2 Class
        (at [ "ttb_sel", "id" ] string)
        (field "text" string)



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
        , url = "http://165.227.134.194:8080/gcall"
        , body = Http.stringBody "text/plain" params
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
