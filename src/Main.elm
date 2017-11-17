import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Http

import Fetcher exposing (getNewestNumber, getTimetable)
import Parser exposing (parse, Timetable, TimetableRow, TimetableCell(Lessons, NoLessons), Lesson(Lesson, Empty))
import Ports

import Task
import Array

main = 
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Flags = 
  { online : Bool
  , json : Maybe String
  }

type alias Model =
  { online : Bool
  , data : Timetable
  , currentDayIndex : Int
  }

init : Flags -> (Model, Cmd Msg)
init flags = 
  case flags.json of
    Just json ->
      (Model flags.online [] 0, send (FromCache json))

    Nothing ->
      (Model flags.online [] 0, send Online)


send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity

-- UPDATE

type Msg 
  = NewContent (Result Http.Error String)
  | FromCache String
  | Online
  | VersionJson (Result Http.Error String)
  | Fetch Int
  | Update
  | NextDay
  | PrevDay


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NewContent (Ok content) ->

      -- returned JSON is embedded in a call to JS function, so we strip out unnecessary characters from both sides
      let
        newContent = content |> String.dropLeft 103 |> String.dropRight 43

        newData = parse newContent
      in
        ({ model | data = newData }, Cmd.batch [ store content, Cmd.none] )

    NewContent (Err err) ->
      (model, Cmd.none)

    FromCache json ->
      let 
        newContent = json |> String.dropLeft 103 |> String.dropRight 43

        newData = parse newContent
      in
        ({ model | data = newData }, Cmd.none)

    Online ->
      (model, getNewestNumber VersionJson)

    VersionJson (Ok json) ->
      (model, send (Fetch (Parser.globalUpdateParser json)))

    VersionJson (Err xd) ->
      (model, Cmd.none)

    Fetch num ->
      (model, getTimetable NewContent num)

    Update ->
      (model, send Online)

    PrevDay ->
      ({ model | currentDayIndex = max (model.currentDayIndex - 1) 0}, Cmd.none)
          
    NextDay ->
      ({ model | currentDayIndex = min (model.currentDayIndex + 1) 4}, Cmd.none)
          



store : String -> Cmd msg
store str =
  Ports.saveInLocalStorage str

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
    [ button [ onClick PrevDay ] [ text "<-" ]
    , button [ onClick NextDay ] [ text "->" ]
    , displayTable model.currentDayIndex model.data
    , (if model.online then
         button [ onClick Update ] [ text "Pobierz nowa zawartosc" ]
       else
         p [] [ text "Jestes offline" ])
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- helper view

displayTable : Int -> Timetable -> Html msg
displayTable index timetable =
  div [] 
    [ tableRow (Maybe.withDefault [] (Array.get index (Array.fromList timetable))) ]


tableRow : TimetableRow -> Html msg
tableRow row = 
  div [ style [("display", "flex")] ]
    (List.map tableCell row)

tableCell : TimetableCell -> Html msg
tableCell cell =
  let 
    css =
      [ ("display", "flex")
      , ("flex-direction", "column")
      , ("float", "left")
      , ("height", "200px")
      , ("width", "200px")
      ]

    content = 
      case cell of 
        Lessons lessons ->
          lessons

        NoLessons ->
          []
  in 
  div [ style css ]
    (List.map displayLesson content)


displayLesson lesson =
  let 
    css =
        [ ("flex", "1 1 0")
        , ("border", "1px solid black")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("justify-content", "center")
        , ("align-items", "center")
        , ("text-align", "center")
        ]

    go : List (Html msg)
    go =
      case lesson of
        Lesson {subject, teacher, classroom} ->
          [ text subject.name
          , br [] []
          , text (teacher.firstname ++ " " ++ teacher.lastname)
          , br [] []
          , text classroom.name
          ]
          
        Empty -> 
          [p [] []]
        
  in
    div [ style css ] go

