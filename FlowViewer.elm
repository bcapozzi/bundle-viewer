import StartApp
import Task
import Html exposing (..)
import Effects exposing (Effects,Never)
import Http
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import GeoUtils exposing (..)
import Set

availableProblemsUrl = "http://localhost:3008/api/flow_viz/availableProblems"

airportLocationUrl airportId = 
  ("http://localhost:3008/api/flow_debug/resourceLocation/" ++ airportId)

availableLayersUrl airport problemID = 
  ("http://localhost:3008/api/flow_viz/layersAvailable/" ++ airport ++ "/" ++ problemID)

inputRoutesUrl problemID = 
  ("http://localhost:3008/api/flow_viz/inputRoutes/" ++ problemID)

flowSegmentsUrl problemID algorithm = 
  ("http://localhost:3008/api/flow_viz/flowSegments/" ++ problemID ++ "/" ++ algorithm)

type alias Model =
  { problems : Maybe Problems
  , selectedAirport : String
  , selectedAirportLocation : Maybe GeoPoint2D
  , problemsForSelectedAirport : List String
  , selectedProblemID : String
  , layersAvailable : List String
  , selectedLayer : String
  , inputRoutes : Maybe InputRoutes
  }

type alias Algorithms = List Algorithm

type alias Algorithm = 
  { name : String }

type alias InputRoutes = List InputRoute

type alias InputRoute = 
  {
    flightID : String
  , geometry : String
  }

type alias FlowSegments = List FlowSegment
type alias FlowSegment = 
  { id : String
  , geometry : String
  }


type alias Problems = List Problem
type alias Problem = 
  { 
    id : Int
  , airport : String
  }

app = 
  StartApp.start 
    { 
      init = init
    , view = view
    , update = update
    , inputs = []
    }

main = 
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

init = 
  ({ problems = Nothing
   , selectedAirport = "NONE"
   , selectedAirportLocation = Nothing
   , problemsForSelectedAirport = []
   , selectedProblemID = "NONE"
   , layersAvailable = []
   , selectedLayer = "NONE"
   , inputRoutes = Nothing}, Effects.none)

view address model = 
  div []
    [ div [] [button [(onClick address GetAvailableProblems)] [Html.text "Click to get available problems!"]]
    , div [] (viewAirportsAvailable model address)
    , div [] (viewProblemsAvailableForAirport model address)
    , div [] (viewLayersAvailableFor model address)
    , div [] (viewInputRoutes model)
    , div [] (viewSelectedAirportLocation model)
    ]
    

type Action 
  = GetAvailableProblems
  | UpdateAvailableProblems (Maybe Problems)
  | UpdateSelectedAirportLocation (Maybe GeoPoint2D)
  | GetProblemsAvailableForAirport String
  | GetLayersAvailableForProblem String
  | UpdateAvailableLayers (Maybe Algorithms)
  | UpdateSelectedLayer String
  | UpdateInputRoutes (Maybe InputRoutes)
  | UpdateFlowSegments (Maybe FlowSegments)

update action model = 
  case action of 
    GetAvailableProblems ->
      (model, getAvailableProblems)
    UpdateSelectedAirportLocation maybeLocation ->
      ({model | selectedAirportLocation = maybeLocation}, Effects.none)
    UpdateAvailableProblems maybeProblems ->
      ({model | problems = maybeProblems}, Effects.none) 
    GetProblemsAvailableForAirport airport ->
      ({model | selectedAirport = airport, problemsForSelectedAirport = (selectProblemsForAirport model airport)}, (getSelectedAirportLocation airport))
    GetLayersAvailableForProblem problemID ->
      ({model | selectedProblemID = problemID}, (getLayersAvailableForProblem model.selectedAirport problemID))
    UpdateAvailableLayers maybeAlgorithms ->
      case maybeAlgorithms of 
        Nothing ->
          (model, Effects.none)
        Just algorithms ->
          let
            names = List.map (\a -> a.name) algorithms
          in
            ({model | layersAvailable = ("InputRoutes" :: names)}, Effects.none)
    UpdateSelectedLayer layer ->
      ({model | selectedLayer = layer}, (getContentsForLayer model.selectedProblemID layer))
    UpdateInputRoutes maybeInputRoutes ->
      ({model | inputRoutes = maybeInputRoutes}, Effects.none)
    UpdateFlowSegments maybeFlowSegments ->
      (model, Effects.none)

selectProblemsForAirport model selectedAirport = 
  case model.problems of
    Nothing ->
      []
    Just problems ->
      let
        problemsForAirport = (List.filter (\p -> (p.airport ==  selectedAirport)) problems)
      in
        List.map (\p -> (toString p.id)) problemsForAirport

isForAirport target problem =
  if (target == problem.airport) then
    True
  else
    False

getSelectedAirportLocation airport = 
    Http.get airportLocationDecoder (airportLocationUrl airport)
    |> toMaybeWithLogging
    |> Task.map UpdateSelectedAirportLocation
    |> Effects.task

airportLocationDecoder =
  Decode.object2 GeoPoint2D
    ("lon_degrees" := Decode.float)
    ("lat_degrees" := Decode.float)

getContentsForLayer problemID layer = 
  if (layer == "InputRoutes") then
    Http.get inputRoutesDecoder (inputRoutesUrl problemID)
    |> toMaybeWithLogging
    |> Task.map UpdateInputRoutes
    |> Effects.task
  else
    Http.get flowSegmentsDecoder (flowSegmentsUrl problemID layer)
    |> toMaybeWithLogging
    |> Task.map UpdateFlowSegments
    |> Effects.task

getAvailableProblems : Effects Action
getAvailableProblems = 
  Http.get problemsDecoder availableProblemsUrl
    |> toMaybeWithLogging
    |> Task.map UpdateAvailableProblems
    |> Effects.task

--getLayersAvailableForProblem : Effects Action
getLayersAvailableForProblem airport problemID = 
  Http.get layersDecoder (availableLayersUrl airport problemID)
    |> toMaybeWithLogging
    |> Task.map UpdateAvailableLayers
    |> Effects.task

-- An alternative to Task.toMaybe which dumps error information to the console log
toMaybeWithLogging : Task.Task x a -> Task.Task y (Maybe a)
toMaybeWithLogging task =
  Task.map Just task `Task.onError` (\msg -> Debug.log (toString msg) (Task.succeed Nothing))

flowSegmentsDecoder = 
  Decode.object1 identity
    ("flow_segments" := Decode.list flowSegmentDecoder)

flowSegmentDecoder = 
  Decode.object2 FlowSegment
    ("flow_segment_id" := Decode.string)
    ("geom" := Decode.string)

inputRoutesDecoder =
  Decode.object1 identity
    ("input_routes" := Decode.list inputRouteDecoder)

inputRouteDecoder =
  Decode.object2 InputRoute
    ("flight_id" := Decode.string)
    ("geometry" := Decode.string)

problemsDecoder = 
  Decode.object1 identity
    ("problems" := Decode.list problemDecoder)

problemDecoder = 
  Decode.object2 Problem
    ("problem_id" := Decode.int)
    ("airport_id" := Decode.string)

layersDecoder = 
  Decode.object1 identity
    ("algorithms" := Decode.list algorithmDecoder)

algorithmDecoder = 
  Decode.object1 Algorithm
    ("clustering_strategy" := Decode.string)



viewAirportsAvailable model address = 
  case model.problems of
    Nothing ->
      [button [][Html.text "none"]]
    Just problems ->
      viewAirports problems model.selectedAirport address 

viewAirports problems currentAirport address =
  let
    airports = List.map (\p -> p.airport) problems
    uniqueAirports = Set.toList (Set.fromList airports)
  in
    List.map (\a -> viewAirport a currentAirport address) uniqueAirports

viewAirport airport currentAirport address = 
  button [(onClick address (GetProblemsAvailableForAirport airport)), style [("background-color",(getColorString airport currentAirport))]] [Html.text airport]

viewProblemsAvailableForAirport model address =
  List.map (\p -> viewProblem p model.selectedProblemID address) model.problemsForSelectedAirport

viewProblem problemID selectedProblemID address = 
  button [(onClick address (GetLayersAvailableForProblem problemID)), style [("background-color",(getColorString problemID selectedProblemID))]] [Html.text problemID]

viewLayersAvailableFor model address = 
  List.map (\layer -> viewLayer layer model.selectedLayer address) model.layersAvailable

viewLayer layer selectedLayer address = 
  button [(onClick address (UpdateSelectedLayer layer)), style [("background-color",(getColorString layer selectedLayer))]] [Html.text layer]

viewInputRoutes model = 
  case model.inputRoutes of
    Nothing ->
      [button [][Html.text "NO ROUTES"]]
    Just inputRoutes ->
--      let
--        refGeoCoord = getReferencePoint model.selectedAirport
      List.map (\r -> viewInputRoute r) inputRoutes

viewInputRoute inputRoute =
  button [][Html.text inputRoute.flightID]

viewSelectedAirportLocation model =
  case model.selectedAirportLocation of
    Nothing -> 
      [button [][Html.text "NO SELECTED AIRPORT LOCATION DEFINED"]]
    Just location ->
      [button [][Html.text ((toString location.latDeg) ++ "," ++ (toString location.lonDeg))]]

getColorString resourceId currentResourceId = 
  if resourceId == currentResourceId then
    "red"
  else
    "gray"

getReferencePoint maybeResourceLocation = 
  case maybeResourceLocation of
    Nothing ->
      {latDeg = 0, lonDeg = 0}
    Just resourceLocation ->
      {latDeg = resourceLocation.latDeg, lonDeg = resourceLocation.lonDeg}
