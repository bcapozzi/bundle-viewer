module GeoUtils where

type alias GeoPoint2D = 
  { latDeg: Float,
    lonDeg: Float
  }

toRadians deg = 
  deg*pi/180.0

toDegrees rad =
  rad*180.0/pi

degreesToNmi deg =
  deg * 60.0

toXY geoRef geoCoord = 
  let
    p1 = toPositiveWestLongitude geoRef
    p2 = toPositiveWestLongitude geoCoord
  in
    getEastNorthOffsetNmiBetween p1 p2
   
toPositiveWestLongitude geoCoord = 
  { latDeg = geoCoord.latDeg
  , lonDeg = -geoCoord.lonDeg
  }

-- this assumes that longitude is positive west
-- and that latitude is positive to the north
getDistanceBetween p1 p2 =
  let 
    lat1 = toRadians(p1.latDeg)
    lon1 = toRadians(p1.lonDeg)
    lat2 = toRadians(p2.latDeg)
    lon2 = toRadians(p2.lonDeg)
    term1 = (sin((lat1 - lat2) / 2))
    term2 = (sin((lon1 - lon2) / 2))
    d = 2 * asin(sqrt(term1 * term1 + cos(lat1) * cos(lat2) * term2 * term2))
  in
    degreesToNmi (toDegrees d)

-- returns radians between [0,2*pi)
getAzimuthBetween p1 p2 =
  let
    lat1 = toRadians(p1.latDeg)
    lon1 = toRadians(p1.lonDeg)
    lat2 = toRadians(p2.latDeg)
    lon2 = toRadians(p2.lonDeg)
    arg1 = sin(lon1 - lon2) * cos(lat2)
    arg2 = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(lon1 - lon2)
    tc1 = mod (atan2 arg1 arg2) (2*pi)
  in
    tc1

mod y x = 
  (y - (x * toFloat(floor (y / x))))

getEastNorthOffsetNmiBetween p1 p2 =
  let
    dnmi = getDistanceBetween p1 p2
    az = getAzimuthBetween p1 p2
  in
    (dnmi*sin(az), dnmi*cos(az))