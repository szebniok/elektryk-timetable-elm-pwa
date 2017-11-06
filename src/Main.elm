import Html exposing (..)
import Html.Events exposing (..)
import Http

main = 
  Html.program 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { content : String }

init = 
  (Model "click the button", Cmd.none)


-- UPDATE

type Msg 
  = Download
  | NewContent (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Download ->
      (model, getContent)

    NewContent (Ok content) ->
      ({content = content}, Cmd.none)

    NewContent (Err err) ->
      ({content = toString err}, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ p [] [ text (toString model) ] 
    , button [ onClick Download ] [ text "download" ]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- HTTP

getContent : Cmd Msg
getContent =
  Http.send NewContent test

test : Http.Request String
test =
  Http.request
    { method = "POST"
    , headers = headers
    , url = "https://cors-anywhere.herokuapp.com/https://elektryk.edupage.org/gcall"
    , body = Http.stringBody "text/plain" params
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

params : String
params = "gadget=MobileTimetableBrowser&jscid=gi40229&gsh=6bcf1a53&action=reload&num=13&oblast=trieda&id=-52&_LJSL=2048"

type Parameter = Parameter String String

parameters : List Parameter
parameters = 
  [ Parameter "gadget" "MobileTimetableBrowser"
  , Parameter "jscid" "gi9195"
  , Parameter "gsh" "06a16bc7"
  , Parameter "action" "reload"
  , Parameter "num" "13"
  , Parameter "oblast" "trieda"
  , Parameter "id" "-52"
  , Parameter "_LJSL" "2048"
  ]


headers : List Http.Header
headers =
--  [ Http.header "Host" "elektryk.edupage.org"
--  , Http.header "User-Agent" "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:58.0) Gecko/20100101 Firefox/58.0"
  [ Http.header "Accept" "*/*"
  , Http.header "Accept-Language" "pl,en-US;q=0.7,en;q=0.3"
--  , Http.header "Referer" "https://elektryk.edupage.org/mobile/"
  , Http.header "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"
  , Http.header "X-Requested-With" "XMLHttpRequest"
--  , Http.header "Connection" "keep-alive"
  ]