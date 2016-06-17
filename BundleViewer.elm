module BundleViewer where 

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
import GeoUtils

resourcesUrl = "http://localhost:3008/api/flow_debug/resources"

jobsUrl resourceId = 
  ("http://localhost:3008/api/flow_debug/jobs/" ++ resourceId)

flowSegmentsUrl job = 
  ("http://localhost:3008/api/flow_debug/flows/" ++  job.resourceId ++ "/" ++ job.id)

resourceLocationUrl resourceId = 
  ("http://localhost:3008/api/flow_debug/resourceLocation/" ++ resourceId)

type Action 
  = GetResources
  | ShowResources (Maybe Resources)
  | GetJobsAvailableForResource String
  | ShowJobsAvailable (Maybe Jobs)
  | GetFlowSegmentsForJob Job
  | ShowFlowSegments (Maybe FlowSegments)
  | UpdateResourceLocation (Maybe GeoCoord)

type alias Resources = 
  List Resource

type alias Resource = 
  String

type alias Jobs = List Job
type alias Job = 
  { id : String
  , resourceId: String
  }

type alias FlowSegments = List FlowSegment
type alias FlowSegment = 
  { id : String
  , geometry : String
  , numEnteringAtStart: Int
  , numJoiningInternal: Int
  }

type alias GeoCoord = 
  {
    lonDeg: Float
  , latDeg: Float
  }

type alias Model = 
  {
    resources : Maybe Resources
  , jobsAvailable : Maybe Jobs
  , resourceId : String
  , jobId : String
  , flowSegments : Maybe FlowSegments
  , resourceLocation : Maybe GeoCoord
  }

init = 
  ({ resources = Nothing
   , jobsAvailable = Nothing
   , resourceId = "Undefined"
   , jobId = "Undefined"
   , flowSegments = Nothing
   , resourceLocation = Nothing}, Effects.none)

update action model =
  case action of 
    GetResources ->
      ({model | resources = Nothing }, getResources)
    ShowResources maybeResources ->
      ({model | resources = maybeResources}, Effects.none)
    GetJobsAvailableForResource id ->
      ({model | resourceId = id}, getJobsAvailable id) 
    ShowJobsAvailable maybeJobs ->
      ({model | jobsAvailable = maybeJobs, flowSegments=Nothing, jobId = "Undefined"}, (getResourceLocation model.resourceId))
    GetFlowSegmentsForJob job ->
      ({model | jobId = job.id}, (getFlowSegmentsForJob job))
    ShowFlowSegments maybeFlowSegments ->
      ({model | flowSegments = maybeFlowSegments}, Effects.none)
    UpdateResourceLocation maybeGeoCoord ->
      ({model | resourceLocation = maybeGeoCoord}, Effects.none)

getResourceLocation id = 
  Http.get resourceLocationDecoder (resourceLocationUrl id)
    |> toMaybeWithLogging
    |> Task.map UpdateResourceLocation
    |> Effects.task

resourceLocationDecoder =
  Decode.object2 GeoCoord
    ("lon_degrees" := Decode.float)
    ("lat_degrees" := Decode.float)

getFlowSegmentsForJob : Job -> Effects Action
getFlowSegmentsForJob job = 
  Http.get flowSegmentsDecoder (flowSegmentsUrl job)
    |> toMaybeWithLogging
    |> Task.map ShowFlowSegments
    |> Effects.task

flowSegmentsDecoder : Decoder FlowSegments
flowSegmentsDecoder = 
  Decode.object1 identity
    ("flow_segments" := Decode.list flowSegmentDecoder)

flowSegmentDecoder : Decoder FlowSegment
flowSegmentDecoder = 
  Decode.object4 FlowSegment
    ("flow_segment_id" := Decode.string) 
    ("geom" := Decode.string)
    ("num_entering_at_start" := Decode.int)
    ("num_joining_internal" := Decode.int)

geomDecoder = 
  Decode.object1 identity 
    ("coordinates" := Decode.list coordDecoder)

coordDecoder = 
  Decode.tuple2 (,) Decode.float Decode.float
  
getJobsAvailable : String -> Effects Action
getJobsAvailable id = 
  Http.get jobsDecoder (jobsUrl id)
    |> toMaybeWithLogging
    |> Task.map ShowJobsAvailable
    |> Effects.task

jobsDecoder : Decoder Jobs
jobsDecoder = 
  Decode.object1 identity
    ("jobs" := Decode.list jobDecoder)

jobDecoder : Decoder Job
jobDecoder = 
  Decode.object2 Job
    ("job_id" := Decode.string)
    ("resource_id" := Decode.string)


getResources : Effects Action
getResources = 
  Http.get resourcesDecoder resourcesUrl
    |> toMaybeWithLogging
    |> Task.map ShowResources
    |> Effects.task

resourcesDecoder : Decoder Resources
resourcesDecoder =
  Decode.object1 identity
    ("resources" := Decode.list resourceDecoder)

resourceDecoder : Decoder Resource
resourceDecoder =
  Decode.object1 identity
    ("resource_id" := Decode.string)


-- An alternative to Task.toMaybe which dumps error information to the console log
toMaybeWithLogging : Task.Task x a -> Task.Task y (Maybe a)
toMaybeWithLogging task =
  Task.map Just task `Task.onError` (\msg -> Debug.log (toString msg) (Task.succeed Nothing))


view address model = 
  div []
    [ div [] [button [(onClick address GetResources)] [Html.text "Click to get resources!" ]]
    , div [] (viewResourcesAvailable model address)
    , div [] (viewJobsAvailable model address)
    , div [] (viewFlowSegmentsAvailable model)
    , div [] (viewResourceLocation model)
    ]
    
viewResourceLocation model = 
  case model.resourceLocation of
    Nothing ->
      [li [][Html.text "NO RESOURCE LOCATION"]]
    Just resourceLocation ->
      [li [][Html.text ((toString resourceLocation.lonDeg) ++ "," ++ (toString resourceLocation.latDeg))]]

viewFlowSegmentsAvailable model = 
  case model.flowSegments of
    Nothing ->
      [Html.fromElement (collage 800 600 [drawBorder 800 600])]
    Just flowSegments ->
      viewFlowSegments flowSegments model

viewFlowSegments flowSegments model = 
  let
    segmentLines = List.map (\f -> drawFlowSegment f model.resourceLocation) flowSegments
  in
    [Html.fromElement (collage 800 600 ( (drawOrigin model.resourceId) :: ((drawBorder 800 600) :: segmentLines)) )]

drawOrigin resourceId = 
  (move (0,0) (filled blue (circle 4)))

drawBorder width height = 
  (filled (rgba 200 200 200 0.3) (rect width height))

drawFlowSegment flowSegment maybeResourceLocation = 
  let
    coords = parseGeometry flowSegment.geometry
    ref = getReferencePoint maybeResourceLocation
    coordsXY = List.map (\gc -> toDisplayXY ref gc) coords
  in
    (traced (solid blue) (path coordsXY))

getReferencePoint maybeResourceLocation = 
  case maybeResourceLocation of
    Nothing ->
      {latDeg = 0, lonDeg = 0}
    Just resourceLocation ->
      {latDeg = resourceLocation.latDeg, lonDeg = resourceLocation.lonDeg}


toDisplayXY geoRef geoTuple = 
  let
    (lon,lat) = geoTuple
    xy_nmi = GeoUtils.toXY geoRef {latDeg = lat, lonDeg = lon}
    (x_nmi, y_nmi) = xy_nmi
  in
    (x_nmi, y_nmi)

viewFlowSegment flowSegment = 
  li [] [Html.text (flowSegment.id ++ " , " ++ flowSegment.geometry)]

parseGeometry geometry = 
  let 
    pairList = String.split "," (String.dropRight 1 (String.dropLeft 11 geometry))
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

viewJobsAvailable model address =
  case model.jobsAvailable of
    Nothing ->
      [button [][Html.text "zilch"]]
    Just jobs ->
      viewJobs jobs address model.jobId

viewJobs jobs address currentJobId = 
  List.map (\j -> viewJob address j currentJobId) jobs

viewJob address job currentJobId =
  button [(onClick address (GetFlowSegmentsForJob job)), style [("background-color", (getColorString job.id currentJobId))]] [Html.text job.id]

viewResourcesAvailable model address = 
  case model.resources of
    Nothing ->
      [button [][Html.text "none"]]
    Just resources ->
      viewResources resources address model.resourceId

viewResources resources address currentResourceId = 
  List.map (\r -> viewResource address r currentResourceId) resources

viewResource address r currentResourceId = 
  button [(onClick address (GetJobsAvailableForResource r)),style [("background-color",(getColorString r currentResourceId))] ][Html.text r]

getColorString resourceId currentResourceId = 
  if resourceId == currentResourceId then
    "red"
  else
    "gray"

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
