import Html exposing (Html, Attribute, button, p, node, div, text, table, tr, td, h1, h2, h3, h4, h5, h6, ul, li, br, input)
import Html.App
import Html.Attributes exposing (style, class, rel, href, type', placeholder, value, src)
import Html.App as Html
import Html.Events exposing ( onClick, onMouseOver, onInput )
import List exposing (map)
import Dict exposing (Dict)
import String
import Time exposing (Time, second)
import Json.Decode as Json
import Http
import Task
import Svg
import Svg.Attributes exposing (cx, cy, r, height, width, viewBox, color, fill)

type Status = Good | Bad | Warning | Empty

type alias Date = Int

type alias DetailedStatus =
  { status : Status
  , date : Date
  , filesInError: List String
  , numberOfLines: Int
  , numberOfFiles: Int
  }

type alias FtpStatus =
  { status : Status
  , downloadSpeed : Maybe Float
  }

type alias AllStatus = Dict String (FtpStatus, Dict String (List DetailedStatus))


lastDate : List DetailedStatus -> Maybe Date
lastDate xs = List.maximum (map .date xs)

statusOfDay : List DetailedStatus -> Date -> DetailedStatus
statusOfDay xs d = case List.filter (\x -> x.date == d) xs of
  [] -> { status=Empty, date=d, numberOfLines=0, numberOfFiles=0, filesInError=[] }
  x::xs -> x

normalize : Date -> Int -> List DetailedStatus -> List DetailedStatus
normalize maxDate width xs =
    map (statusOfDay xs) [(maxDate-width+1)..maxDate]

-- Static data. We need to fetch this from the backend
allStatus : AllStatus
allStatus =
  let
    ftp1Status = { status = Good, downloadSpeed = Just 105.0 }
    ftp1Histories = Dict.fromList [
      ("/abc/access_log.*", [{status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Bad,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Warning,date=20160716,numberOfLines=1000,numberOfFiles=3,filesInError=[]}]),
      ("/def/access_log.*", [{status=Good,date=20160711,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Bad,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Warning,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}]),
      ("/rfs/access_log.*", [{status=Good,date=20160701,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160702,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160703,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Bad,date=20160704,numberOfLines=1000,numberOfFiles=3,filesInError=["/rfs/access_log.20160704_1.log"]}])
    ]
    ftp2Status = { status = Good, downloadSpeed = Just 290.0 }
    ftp2Histories = Dict.fromList [
      ("/abc/access_log.*", [{status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Bad,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Bad,date=20160716,numberOfLines=1000,numberOfFiles=3,filesInError=[]}]),
      ("/def/access_log.*", [{status=Good,date=20160711,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}])
    ]
    ftp3Status = { status = Good, downloadSpeed = Just 500.0 }
    ftp3Histories = Dict.fromList [
      ("/abc/access_log.*", [{status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160716,numberOfLines=1000,numberOfFiles=3,filesInError=[]}]),
      ("/def/access_log.*", [{status=Good,date=20160711,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160712,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160713,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160714,numberOfLines=1000,numberOfFiles=3,filesInError=[]}, {status=Good,date=20160715,numberOfLines=1000,numberOfFiles=3,filesInError=[]}])
    ]
  in
    Dict.fromList [
      ("iheart.akamai.net", (ftp1Status, ftp1Histories)),
      ("war.str3am.com", (ftp2Status, ftp2Histories)),
      ("wamu-2.streamguys.com", (ftp3Status, ftp3Histories))
      ]


-- APP
main : Program Never
main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL
type alias Model = { days: Int, status : Maybe DetailedStatus, displayErrorsOnly : Bool, query : Maybe String, gifUrl :  String, refreshOk : Bool }

model : Model
model = { days=18, status=Nothing, displayErrorsOnly=True, query=Nothing, gifUrl="waiting.gif", refreshOk=False }

init : ( Model, Cmd Msg )
init = ( model, getRandomGif "cat" )


-- UPDATE
type Msg = Over DetailedStatus | ToggleErrorDisplay | Search String | Tick | FetchFail Http.Error | FetchSucceed String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Over status -> ({ model | status = Just status }, Cmd.none)
    ToggleErrorDisplay -> ({ model | displayErrorsOnly = not model.displayErrorsOnly }, Cmd.none)
    Search string -> ({ model | query = if (String.length string > 0) then Just(string) else Nothing }, Cmd.none)
    Tick -> ({ model | days = model.days+1 }, getRandomGif "cat")
    FetchSucceed newUrl -> ({ model | gifUrl = newUrl, refreshOk=True }, Cmd.none)
    FetchFail _ -> ({ model | refreshOk=False }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Time.every (600 * second) (\_ -> Tick)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decoder String
decodeGifUrl = Json.at ["data", "image_url"] Json.string

-- VIEW
statusRow title xs = tr [] ([td [class "StatusTitle"] [text title]] ++ map statusCell xs)

mouseOverCell cl status =
  let tooltip = (toString status.numberOfLines) ++ " lines from " ++ (toString status.numberOfFiles) ++ " files"
  in td [onMouseOver (Over status), Html.Attributes.title tooltip] [withStatusDot status.status [text ""]]

statusCell status = case status.status of
  Empty -> td [Html.Attributes.title "No sync this day"] [withStatusDot Empty [text ""]]
  other -> mouseOverCell (statusToClass other) status

statusToClass : Status -> String
statusToClass s = toString s

ftpStatusDiv : String -> FtpStatus -> Html a
ftpStatusDiv ftpUrl ftpStatus =
  let speedString = case ftpStatus.downloadSpeed of
    Just speed -> "Speed: " ++ toString speed ++ "kB/s"
    Nothing -> "No speed"
  in
    div [ class "FtpStatus" ] [
      withStatusDot ftpStatus.status [text (ftpUrl ++ ". " ++ speedString)]
    ]


-- Create a table of FTP/FG status.
-- Each table row represents a FTP/FG history. Each column represents a day.
statusTable : Date -> Int -> Dict String (List DetailedStatus) -> Html Msg
statusTable maxDate width histories = let
  rows = map (\(fg,xs) -> statusRow fg (normalize maxDate width xs)) (Dict.toList histories)
  in table [] rows

-- Create the status view for an FTP (ftp status + ftp/fg status)
statusTableWithFtpStatus : Date -> Int -> String -> FtpStatus -> Dict String (List DetailedStatus) -> Html Msg
statusTableWithFtpStatus maxDate width ftpUrl ftpStatus fgStatus =
  div [] [
    ftpStatusDiv ftpUrl ftpStatus,
    statusTable maxDate width fgStatus
  ]

-- Create one section by FTP
mkAllStatusTablesWithFtpStatus maxDate width allStatus =
  let
    statusTableByFtp =
      Dict.map (\ftpUrl (ftpStatus, fgStatus) ->
        statusTableWithFtpStatus maxDate width ftpUrl ftpStatus fgStatus) allStatus
  in
    div [] (List.intersperse (div [] [br [] []]) (Dict.values statusTableByFtp))

statusDetailsDiv : DetailedStatus -> Html a
statusDetailsDiv status =
  let
    errors = ul [] (map (\f -> li [] [text ("File in error: " ++ f)]) status.filesInError)
    lines = toString status.numberOfLines ++ " lines so far from " ++ toString status.numberOfFiles ++ " files."
    date = toString status.date
  in
    div [] [
      text ("Sync state (" ++ date ++ "): " ++ toString status.status ++ ". " ++ lines),
      errors
      ]

hasError : List DetailedStatus -> Bool
hasError xs = List.any (\status -> status.status == Bad || status.status == Warning) xs

filterRows : (String -> FtpStatus -> String -> List DetailedStatus -> Bool) -> AllStatus -> AllStatus
filterRows pred allStatus =
  let filtered = Dict.map (\ftp (ftpStatus, fgStatus) -> (ftpStatus, Dict.filter (\pattern statusList -> pred ftp ftpStatus pattern statusList) fgStatus)) allStatus
  in Dict.filter (\_ (_, fgStatus) -> not (Dict.isEmpty fgStatus)) filtered

filterStatus : AllStatus -> AllStatus
filterStatus allStatus = filterRows (\_ _ _ statusList -> hasError statusList) allStatus

filterSearch : String -> AllStatus -> AllStatus
filterSearch queryString allStatus = filterRows (\ftp _ pattern _ -> String.contains queryString pattern || String.contains queryString ftp) allStatus

view : Model -> Html Msg
view model =
  let
    searchedStatus = case model.query of
      Just queryString -> filterSearch queryString allStatus
      Nothing -> allStatus
    filteredStatus = if model.displayErrorsOnly then filterStatus searchedStatus else searchedStatus
    maxDate = let
      fgStatus = map (\x -> Dict.values (snd x)) (Dict.values filteredStatus)
      in lastDate (List.concat (List.concat fgStatus))
  in
    div [ class "container" ] [
      css "css/style.css",
      css "css/font-awesome.min.css",
      css "css/bootstrap.min.css",
      h1 [] [text "Log collection"],
      case model.refreshOk of
        False -> p [] [ text "Not fresh" ]
        True -> p [] [ text "Fresh" ],
      Html.img [src model.gifUrl] [],
      input [ class "SearchBox", placeholder "Search", onInput Search ] [],
      button [ class "btn btn-primary btn-md", onClick ToggleErrorDisplay ] [text (if model.displayErrorsOnly then "Display All" else "Display Errors Only")],
      div [ class "row", class "jumbotron" ] [
        case maxDate of
          Nothing -> p [] [ text "No status to display" ]
          Just m -> div [] [
            mkAllStatusTablesWithFtpStatus m model.days filteredStatus
            , p [] [ text "" ]
            , case model.status of
              Nothing -> p [] [ text "" ]
              Just st -> p [] [ statusDetailsDiv st ]
          ]
      ]
  ]

withDot colour x = div [] ([
  Svg.svg [ width "20", height "20", viewBox "0 0 20 20" ] [ Svg.circle [ cx "10", cy "10", r "10", fill colour ] [text ""]]
  ] ++ x)

withStatusDot s = case s of
  Good -> withDot "green"
  Bad -> withDot "red"
  Warning -> withDot "orange"
  Empty -> withDot "blue"

css : String -> Html msg
css path =
  node "link" [ rel "stylesheet", href path ] []
