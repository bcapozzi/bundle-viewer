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
import Text

artccBoundariesUrl = "http://localhost:3008/api/flow_viz/artccBoundaries"

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
  , layersAvailable : List FlowLayer
  , selectedLayer : String
  , inputRoutes : Maybe InputRoutes
  , flowSegments : Maybe FlowSegments
  , displayWidthNMI : Float
  , displayHeightNMI : Float
  , displayOriginX : Float
  , displayOriginY : Float
  , artccBoundaries : Maybe ArtccBoundaries
  }

type alias ArtccBoundaries = List ArtccBoundary

type alias ArtccBoundary = 
  { name : String
  , id : String
  , geometry : String 
  }

type alias FlowLayer = 
  {
    name : String
  , isSelected : Bool
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
  { id : Int
  , contributorID : Int
  , geometry : String
  , contributorGeometry : String
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
   , inputRoutes = Nothing
   , flowSegments = Nothing
   , displayWidthNMI = 800.0
   , displayHeightNMI = 600.0
   , displayOriginX = 0.0
   , displayOriginY = 0.0
   , artccBoundaries = Nothing}, Effects.none)

view address model = 
  div []
    [ div [] [button [(onClick address GetArtccBoundaries)] [Html.text "Click to get ARTCC boundaries"]]
    , div [] [button [(onClick address GetAvailableProblems)] [Html.text "Click to get available problems!"]]
    , div [] (viewAirportsAvailable model address)
    , div [] (viewProblemsAvailableForAirport model address)
    , div [] (viewLayersAvailableFor model address)
    , div [] (viewSelectedAirportLocation model)
    , div [] (viewFlowLayers model)
    , div [] (viewArtccBoundaries model)
    , div [] [button [onClick address ZoomIn][Html.text "Zoom In"], button [onClick address ZoomOut][Html.text "Zoom Out"]]
    , div [] [button [onClick address PanLeft][Html.text "Pan Left"], button [onClick address PanRight][Html.text "Pan Right"],  button [onClick address PanUp][Html.text "Pan Up"], button [onClick address PanDown][Html.text "Pan Down"]]
    ]
    

type Action 
  = GetAvailableProblems
  | UpdateSelectedAirport String
  | UpdateAvailableProblems (Maybe Problems)
  | UpdateSelectedAirportLocation (Maybe GeoPoint2D)
  | GetProblemsAvailableForAirport String
  | UpdateSelectedProblem String
  | GetLayersAvailableForProblem String
  | UpdateAvailableLayers (Maybe Algorithms)
  | UpdateSelectedLayer String
  | UpdateInputRoutes (Maybe InputRoutes)
  | UpdateFlowSegments (Maybe FlowSegments)
  | ZoomIn 
  | ZoomOut
  | PanLeft
  | PanRight
  | PanUp
  | PanDown
  | GetArtccBoundaries
  | UpdateArtccBoundaries (Maybe ArtccBoundaries)

update action model = 
  case action of
    GetArtccBoundaries ->
      (model, getArtccBoundaries)
    UpdateArtccBoundaries maybeBoundaries ->
      ({model | artccBoundaries = maybeBoundaries}, Effects.none)
    ZoomIn ->
      ({model | displayWidthNMI = model.displayWidthNMI*0.9
              , displayHeightNMI = model.displayHeightNMI*0.9}, Effects.none)
    ZoomOut ->
      ({model | displayWidthNMI = model.displayWidthNMI*1.1
              , displayHeightNMI = model.displayHeightNMI*1.1}, Effects.none)
    PanLeft ->
      ({model | displayOriginX = model.displayOriginX + 0.1*800},  Effects.none)
 
    PanRight ->
      ({model | displayOriginX = model.displayOriginX - 0.1*800},  Effects.none)
 
    PanUp ->
      ({model | displayOriginY = model.displayOriginY - 0.1*600},  Effects.none)
 
    PanDown ->
      ({model | displayOriginY = model.displayOriginY + 0.1*600},  Effects.none)
 
    GetAvailableProblems ->
      (model, getAvailableProblems)
    UpdateSelectedAirport airport ->
      ({model | selectedAirport = airport, problemsForSelectedAirport = (selectProblemsForAirport model airport), inputRoutes=Nothing, flowSegments=Nothing, selectedProblemID = "NONE", selectedLayer = "NONE", layersAvailable=[]}, getSelectedAirportLocation airport)
    UpdateSelectedAirportLocation maybeLocation ->
      ({model | selectedAirportLocation = maybeLocation}, Effects.none)
    UpdateAvailableProblems maybeProblems ->
      ({model | problems = maybeProblems}, Effects.none) 
    UpdateSelectedProblem problemID ->
      ({model | selectedProblemID = problemID
              , inputRoutes = Nothing
              , flowSegments = Nothing
              , selectedLayer = "NONE"}, (getLayersAvailableForProblem model.selectedAirport problemID))
    GetProblemsAvailableForAirport airport ->
      ({model | selectedAirport = airport, problemsForSelectedAirport = (selectProblemsForAirport model airport)}, Effects.none)
    GetLayersAvailableForProblem problemID ->
      ({model | selectedProblemID = problemID}, (getLayersAvailableForProblem model.selectedAirport problemID))
    UpdateAvailableLayers maybeAlgorithms ->
      case maybeAlgorithms of 
        Nothing ->
          (model, Effects.none)
        Just algorithms ->
          let
            names = List.map (\a -> {name = a.name, isSelected = False}) algorithms
          in
            ({model | layersAvailable = ({name = "InputRoutes", isSelected = False} :: names)}, Effects.none)
    UpdateSelectedLayer layer ->
      ({model | selectedLayer = layer
              , layersAvailable = (updateLayersGivenSelected layer model.layersAvailable)
              , inputRoutes = (updateInputRoutesGivenSelected model.layersAvailable model.inputRoutes)
              , flowSegments = (updateFlowSegmentsGivenSelected model.layersAvailable model.flowSegments)}, (getContentsForLayer model.selectedProblemID layer))
    UpdateInputRoutes maybeInputRoutes ->
      ({model | inputRoutes = maybeInputRoutes}, Effects.none)
    UpdateFlowSegments maybeFlowSegments ->
      ({model | flowSegments = maybeFlowSegments}, Effects.none)

updateInputRoutesGivenSelected layersAvailable maybeInputRoutes =
  let
    selectedLayers = List.filter (\layer -> (layer.isSelected == True)) layersAvailable
    selectedNames = List.map (\layer -> layer.name) selectedLayers
    inputRoutesSelected = List.member "InputRoutes" selectedNames
  in
    if (inputRoutesSelected == True) then
      maybeInputRoutes
    else
      Nothing

updateFlowSegmentsGivenSelected layersAvailable maybeFlowSegments =
  maybeFlowSegments
  
updateLayersGivenSelected selected layersAvailable =
  List.map (\layer -> if (layer.name == selected) then {name = layer.name, isSelected = True && (not layer.isSelected)} else {name = layer.name, isSelected = (layer.isSelected && True)}) layersAvailable

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

getArtccBoundaries = 
    Http.get artccBoundariesDecoder artccBoundariesUrl
    |> toMaybeWithLogging
    |> Task.map UpdateArtccBoundaries
    |> Effects.task

artccBoundariesDecoder = 
  Decode.object1 identity
    ("artcc_boundaries" := Decode.list artccBoundaryDecoder)

artccBoundaryDecoder = 
  Decode.object3 ArtccBoundary
    ("name" := Decode.string)
    ("ncr_id" := Decode.string) 
    ("geom" := Decode.string)

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
  Decode.object4 FlowSegment
    ("output_flow_segment_id" := Decode.int)
    ("contributor_id" := Decode.int)
    ("flow_geom" := Decode.string) 
    ("contributor_geom" := Decode.string)

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

viewArtccBoundaries model = 
  case model.artccBoundaries of 
    Nothing -> 
      [button [][Html.text "no artcc boundaries"]]
    Just boundaries ->
      [button [][Html.text "some artcc boundaries"]]

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
  button [(onClick address (UpdateSelectedAirport airport)), style [("background-color",(getColorString airport currentAirport))]] [Html.text airport]

viewProblemsAvailableForAirport model address =
  List.map (\p -> viewProblem p model.selectedProblemID address) model.problemsForSelectedAirport

viewProblem problemID selectedProblemID address = 
  button [(onClick address (UpdateSelectedProblem problemID)), style [("background-color",(getColorString problemID selectedProblemID))]] [Html.text problemID]

viewLayersAvailableFor model address =
  List.map (\layer -> viewLayer layer.name model.selectedLayer model.layersAvailable address) model.layersAvailable

viewLayer layer selectedLayer layersAvailable address = 
  button [(onClick address (UpdateSelectedLayer layer)), style [("background-color",(getLayerColorString layer selectedLayer layersAvailable))]] [Html.text layer]

viewFlowLayers model = 
  case model.inputRoutes of 
    Nothing ->
      case model.flowSegments of
        Nothing ->
          let 
            artccPaths = generateArtccBoundaryPaths model
          in
            [Html.fromElement (collage 800 600 artccPaths)]
--          [button [][Html.text "NO ROUTES OR FLOW SEGMENTS"]]
        Just flowSegments ->
          viewFlowSegments model
    Just inputRoutes ->
      case model.flowSegments of
        Nothing ->
           viewInputRoutes model
        Just flowSegments ->
           (viewInputRoutesAndFlowSegments model inputRoutes flowSegments)

viewInputRoutesAndFlowSegments model inputRoutes flowSegments =
  let
    artccPaths = generateArtccBoundaryPaths model
    refGeoCoord = getReferencePoint model.selectedAirportLocation
    routePaths = List.map (\f -> drawInputRoute f refGeoCoord model.displayOriginX model.displayOriginY model.displayWidthNMI model.displayHeightNMI) inputRoutes
    segmentPaths = List.map (\f -> drawFlowSegment f refGeoCoord model.displayOriginX model.displayOriginY model.displayWidthNMI model.displayHeightNMI) flowSegments
  in
    [Html.fromElement (collage 800 600 ((drawOrigin model.selectedAirport model.displayOriginX model.displayOriginY)  ::  ((List.append artccPaths (List.append routePaths segmentPaths)))))]
    

viewInputRoutes model = 
  case model.inputRoutes of
    Nothing ->
      [button [][Html.text "NO ROUTES"]]
    Just inputRoutes ->
      let
        artccPaths = generateArtccBoundaryPaths model
        refGeoCoord = getReferencePoint model.selectedAirportLocation
        routePaths = List.map (\f -> drawInputRoute f refGeoCoord model.displayOriginX model.displayOriginY model.displayWidthNMI model.displayHeightNMI) inputRoutes
      in
        [Html.fromElement (collage 800 600 ((drawOrigin model.selectedAirport model.displayOriginX model.displayOriginY) :: (List.append artccPaths routePaths)))]
--        List.map (\r -> viewInputRoute r) inputRoutes

generateArtccBoundaryPaths model =
  case model.artccBoundaries of 
    Nothing ->
      []
    Just boundaries ->
      let 
        refGeoCoord = getReferencePoint model.selectedAirportLocation
      in 
        List.map (\b -> generateBoundaryPath b refGeoCoord model.displayOriginX model.displayOriginY model.displayWidthNMI model.displayHeightNMI) boundaries

generateBoundaryPath boundary ref originX originY displayWidth displayHeight = 
  let
    coords = parsePolygonGeometry boundary.geometry
    coordsXY = List.map (\gc -> toDisplayXY ref gc originX originY displayWidth displayHeight) coords
  in
    (traced (solid green) (path coordsXY))


viewFlowSegments model = 
  case model.flowSegments of
    Nothing ->
      [button [][Html.text "NO FLOW SEGMENTS"]]
    Just flowSegments ->
      let
        refGeoCoord = getReferencePoint model.selectedAirportLocation
        segmentPaths = List.map (\f -> drawFlowSegment f refGeoCoord model.displayOriginX model.displayOriginY model.displayWidthNMI model.displayHeightNMI) flowSegments
      in
        [Html.fromElement (collage 800 600 ((drawOrigin model.selectedAirport model.displayOriginX model.displayOriginY) :: (segmentPaths)))]
--      [button [][Html.text "SOME FLOW SEGMENTS"]]

drawFlowSegment flowSegment ref originX originY displayWidth displayHeight = 
  let
    coords = parseGeometry flowSegment.geometry
    coordsXY = List.map (\gc -> toDisplayXY ref gc originX originY displayWidth displayHeight) coords
    lineStyle = (solid (rgba 0 0 255 0.3))
    widerStyle = {lineStyle | width = 5}
  in
    (traced widerStyle (path coordsXY))


drawOrigin resourceId originX originY = 
  (move (originX,originY) (filled blue (circle 4)))

getReferencePoint maybeLocation = 
  case maybeLocation of
    Nothing ->
      {lonDeg = -97.0, latDeg = 32.0}
    Just location ->
      location

viewInputRoute inputRoute =
  button [][Html.text inputRoute.flightID]

drawInputRoute route ref originX originY displayWidth displayHeight= 
  let
    coords = parseGeometry route.geometry
    coordsXY = List.map (\gc -> toDisplayXY ref gc originX originY displayWidth displayHeight) coords
  in
    (traced (solid gray) (path coordsXY))

toDisplayXY geoRef geoTuple originX originY displayWidthNMI displayHeightNMI = 
  let
    (lon,lat) = geoTuple
    xy_nmi = GeoUtils.toXY geoRef {latDeg = lat, lonDeg = lon}
    (x_nmi, y_nmi) = xy_nmi
    x_display = originX + x_nmi / displayWidthNMI * displayWidthPixels
    y_display = originY + y_nmi / displayHeightNMI * displayHeightPixels
  in
    (x_display, y_display)

displayWidthPixels = 
  800.0

displayHeightPixels = 
  600.0

parseGeometry geometry = 
  let 
    pairList = String.split "," (String.dropRight 1 (String.dropLeft 11 geometry))
  in
    List.map (\p -> parsePoint p) pairList

parsePolygonGeometry geometry = 
  let 
    pairList = String.split "," (String.dropRight 2 (String.dropLeft 9 geometry))
  in
    List.map (\p -> parsePoint p) pairList

parsePoint pointPair = 
  let
    pointStrings = String.words pointPair
    lonString = getFirst pointStrings
    latString = getLast pointStrings
    lonDegrees = (Result.withDefault 0.0 (String.toFloat lonString))
    latDegrees = (Result.withDefault 0.0 (String.toFloat latString))
  in
    (lonDegrees, latDegrees)

getFirst pair = 
  let
    first = List.head pair
  in 
    case first of
      Nothing ->
        "None"
      Just value ->
        value

getLast pair = 
  let
    second = List.drop 1 pair
  in
    getFirst second

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

getLayerColorString layerName selectedLayerName layersAvailable = 
  let
    isSelected = List.filter (\layer -> (layer.isSelected == True && layer.name == layerName)) layersAvailable
  in
    if ((List.length isSelected) > 0) then
      "red"
    else
      "gray"
  

