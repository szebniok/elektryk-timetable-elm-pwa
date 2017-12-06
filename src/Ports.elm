port module Ports exposing (..)


type alias Key =
    String


type alias Value =
    String


port saveInLocalStorage : ( Key, Value ) -> Cmd msg


port trackPageview : String -> Cmd msg


port readActiveClassData : (Maybe String -> msg) -> Sub msg


type alias ClassName =
    String


port sendActiveClass : ClassName -> Cmd msg
