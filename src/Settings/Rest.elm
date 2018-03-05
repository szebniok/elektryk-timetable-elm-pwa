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
    Http.request
        { method = "GET"
        , headers = headers
        , url = "https://elektrykcache.tk:8080/gcall/metadata"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
