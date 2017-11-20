module Fetcher exposing (getNewestNumber, getTimetable, getSubstitutions)

import Date
import Date.Extra.Config.Config_pl_pl as Config
import Date.Extra.Format
import Http

headers : List Http.Header
headers =
  [ Http.header "Accept" "*/*"
  , Http.header "Accept-Language" "pl,en-US;q=0.7,en;q=0.3"
  , Http.header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
  , Http.header "X-Requested-With" "XMLHttpRequest"
  ]



-- GLOBAL UPDATE
-- global update is used to fetch number that is a version number of the newest timetable

globalUpdateRequest : Http.Request String
globalUpdateRequest =
  let 
    params = "gadget=MobileEdupage&jscid=gi34476&gsh=6bcf1a53&action=globalReload&&_LJSL=2048"
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

getNewestNumber : (Result Http.Error String -> msg) -> Cmd msg
getNewestNumber msg =
  Http.send msg globalUpdateRequest



-- TIMETABLE 

timetableRequest : Int -> Http.Request String
timetableRequest num =
  let 
    params = "gadget=MobileTimetableBrowser&jscid=gi40229&gsh=6bcf1a53&action=reload&num=" ++ (toString num) ++ "&oblast=trieda&id=-52&_LJSL=2048"
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

getTimetable : (Result Http.Error String -> msg) -> Int -> Cmd msg
getTimetable msg num =
  Http.send msg (timetableRequest num)



-- SUBSTITUTIONS
-- substitutionsRequest : Date.Date -> Http.Request String
substitutionsRequest  =
  let
--    formattedDate = Date.Extra.Format.format Config.config Date.Extra.Format.isoDateFormat date 
    params = "gadget=MobileSubstBrowser&jscid=gi44900&gsh=6bcf1a53&action=date_reload&date=2017-11-20&_LJSL=2048"
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

getSubstitutions : (Result Http.Error String -> msg) -> Cmd msg
getSubstitutions msg
  = Http.send msg substitutionsRequest