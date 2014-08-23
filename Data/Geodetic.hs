{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic (
  GeodeticModel (..),
  ANS, ans, ans',
  WGS84, wgs84, wgs84',
  GRS80, grs80, grs80',
  GeodeticCoordinate (..), refElipsoid, latitude, longitude, height,
  UTM (..), utmZone, utmEasting, utmNorthing, toUTM,
  ECEF (..), coordX, coordY, coordZ,
  gcDist,
  CourseDirection (..),
  gcCourse,
  ) where

import Data.Geodetic.Coordinate
import Data.Geodetic.GeodeticModel
import Data.Geodetic.GreatCircle
import Data.Geodetic.Elipsoids
import Data.Geodetic.UTM


--ms = wgs84 51.969659 7.605286 0
--ums = toUTM $  ms

{-
import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Control.Lens hiding (_1, _2, _3, (*~))
import Data.Typeable



sm :: (Fractional t) => Length t
sm = 1.852 *~ kilo meter



        



dr :: (Floating t) =>
      GeodeticCoordinate m t -> Velocity t -> Time t -> PlaneAngle t -> PlaneAngle t --GeodeticCoordinate m t 
dr fix v t a =
  let l = ((v * t) / sm)
  in l

t = dr ms (160 *~ (kilo meter / hour)) (30 *~ minute) (90 *~ degree)



msgrs :: GeodeticCoordinate GRS80 Double
msgrs = fromGeodetic ms



cmsny = gcCourse CourseWest ms ny
cmshh = gcCourse CourseEast ms hh


ms = wgs84 51.969659 7.605286 0

foo = wgs84 51.969659 23.605286 0

mse = toEcef ms
ms2 = fromEcef WGS84 mse

hh = wgs84 53.543572 10.02502 74
hhg = grs80 53.543572 10.02502 74

fa = gcDist ms hh
fb = gcDist ms $ fromGeodetic hhg

ny = wgs84 40.43 (-74) 0


d1 = ms ^. latitude - ms2 ^.latitude
d2 = ms ^. longitude - ms2 ^.longitude
d3 = ms ^. height - ms2 ^.height

aa :: (RealFloat t, Typeable t, GeodeticModel a, GeodeticModel b) =>
      GeodeticCoordinate a t -> GeodeticCoordinate b t
aa = fromGeodetic . fromEcef GRS80 . toEcef . fromEcef ANS . toEcef

xx :: [GeodeticCoordinate WGS84 Float]
xx = [ wgs84 x y 0 | x <- [0..359] , y <- [-90, 90] ]
xxx = fmap aa  xx

xxxx = zip xx xxx

xd f =
  let ds = map (\(a,b) -> a ^. f - b ^. f) xxxx
  in (minimum ds, maximum ds)
xd1 = xd latitude     
xd2 = xd longitude
xd3 = xd height

-}
