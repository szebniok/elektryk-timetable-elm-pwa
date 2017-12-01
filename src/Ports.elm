port module Ports exposing (..)


type alias Key =
    String


type alias Value =
    String


port saveInLocalStorage : ( Key, Value ) -> Cmd msg


port trackPageview : String -> Cmd msg
