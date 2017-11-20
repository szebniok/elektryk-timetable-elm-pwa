import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Http

import Fetcher exposing (getNewestNumber, getTimetable, getSubstitutions)
import Parser exposing (parse, Timetable, TimetableRow, TimetableCell(Lessons, NoLessons), Lesson(Lesson, Empty), substitutionsParser)
import Ports

import Time
import Task
import Array
import Date
import Date.Extra.Core
import Date.Extra.Facts exposing (dayOfWeekFromWeekdayNumber)
import Date.Extra.I18n.I_pl_pl exposing (dayName)
import TouchEvents

main : Program Flags Model Msg
main = 
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type Page
  = TimetablePage
  | SubstitutionsPage

type alias Flags = 
  { online : Bool
  , json : Maybe String
  }

type alias Model =
  { online : Bool
  , data : Timetable
  , currentDayIndex : Int
  , touchStart : Maybe TouchEvents.Touch
  , page: Page
  , substitutions : String
  }

init : Flags -> (Model, Cmd Msg)
init flags = 
  case flags.json of
    Just json ->
      (Model flags.online Array.empty 0 Nothing TimetablePage "", send (FromCache json))

    Nothing ->
      (Model flags.online Array.empty 0 Nothing TimetablePage "", send Online)


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
  | CurrentTime Time.Time
  | TouchStart TouchEvents.Touch
  | TouchEnd TouchEvents.Touch
  | SetPage Page
  | FetchSubstitutions
  | SubsitutionsFetched (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NewContent (Ok content) ->

      -- returned JSON is embedded in a call to JS function, so we strip out unnecessary characters from both sides
      let
        newContent = content |> String.dropLeft 103 |> String.dropRight 43

        newData = parse newContent
      in
        ({ model | data = newData }, Cmd.batch [ store content, getCurrentDate] )

    NewContent (Err err) ->
      (model, Cmd.none)

    FromCache json ->
      let 
        newContent = json |> String.dropLeft 103 |> String.dropRight 43

        newData = parse newContent
      in
        ({ model | data = newData }, getCurrentDate)

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

    CurrentTime time ->
      let
        hour = round (Time.inHours time) % 24
        date = Date.fromTime time
        day = Date.dayOfWeek date

        dayToDisplay =
          case day of
            Date.Sat -> Date.Mon
            Date.Sun -> Date.Mon
            day -> 
              if hour > 15 then
                if day == Date.Fri then
                  Date.Mon
                else
                  Date.Extra.Core.nextDay day
              else
                day
              
        dayIndex = (Date.Extra.Core.isoDayOfWeek dayToDisplay) - 1

      in
        ({ model | currentDayIndex = dayIndex}, Cmd.none)

    TouchStart pos ->
      ({ model | touchStart = Just pos}, Cmd.none)

    TouchEnd pos ->
      case model.touchStart of
        Just touchStart ->
          let 
            diffX = touchStart.clientX - pos.clientX
            diffY = touchStart.clientY - pos.clientY
          in
            if abs diffY > abs diffX then
              (model, Cmd.none)
            else if abs diffX < 60 then
              (model, Cmd.none)
            else if diffX < 0 then
              { model | touchStart = Nothing}
                |> update PrevDay
            else
              { model | touchStart = Nothing}
                |> update NextDay

        Nothing ->
          (model, Cmd.none)

    SetPage page ->
      ({ model | page = page }, Cmd.none)

    FetchSubstitutions ->
      (model, getSubstitutions SubsitutionsFetched)

    SubsitutionsFetched (Ok data) -> 
      
      ({ model | substitutions = toString <| substitutionsParser data }, Cmd.none)

    SubsitutionsFetched (Err _) ->
      (model, Cmd.none)
          
      

          
getCurrentDate : Cmd Msg
getCurrentDate =
  Task.perform CurrentTime Time.now


store : String -> Cmd msg
store str =
  Ports.saveInLocalStorage str


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div [ TouchEvents.onTouchEvent TouchEvents.TouchStart TouchStart, TouchEvents.onTouchEvent TouchEvents.TouchEnd TouchEnd ] 
    [ page model
    , navigation model.page
    ]


page : Model -> Html Msg
page model =
  case model.page of
    TimetablePage ->
      timetable model 

    SubstitutionsPage ->
      substitutions model


timetable : Model -> Html Msg
timetable model =
  div [ class "page" ] 
    [ h2 [ class "day-of-week" ] 
        [ text (dayName (dayOfWeekFromWeekdayNumber (model.currentDayIndex + 1))) ]
    , displayTable model.currentDayIndex model.data
    , (if model.online then
         button [ onClick Update ] [ text "Pobierz nowa zawartosc" ]
       else
         p [] [ text "Jestes offline" ])
    ]


substitutions : Model -> Html Msg
substitutions model =
  div [ class "page" ]
    [ button [ onClick FetchSubstitutions ] [ text "pobierz" ] 
    , p [] [ text model.substitutions ]
    ]



displayTable : Int -> Timetable -> Html msg
displayTable index timetable =
  div [] 
    [ tableRow (Maybe.withDefault [] (Array.get index timetable)) ]


tableRow : TimetableRow -> Html msg
tableRow row = 
  div [ class "timetable-row" ]
    (List.indexedMap tableCell row)


tableCell : Int -> TimetableCell -> Html msg
tableCell index cell =
  case cell of
    Lessons lessons ->
      div [ class "timetable-cell" ]
        ([ div [ class "timetable-cell-index" ] [ text (toString index) ] ] ++
         (List.map displayLesson lessons))

    -- if there are no lessons in cell at all, return empty node
    NoLessons ->
      text ""


displayLesson : Lesson -> Html msg
displayLesson lesson =
  let 
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
    div [ class "lesson" ] go

navigation : Page -> Html Msg
navigation page = 
  let
    getClass linkPage =
      if page == linkPage then "active" else ""
  in
    nav []
      [ a [ onClick (SetPage TimetablePage), class (getClass TimetablePage) ] [ text "Plan lekcji" ]
      , a [ onClick (SetPage SubstitutionsPage), class (getClass SubstitutionsPage) ] [ text "Zastępstwa" ]
      ]