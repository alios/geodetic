{-# LANGUAGE DeriveDataTypeable #-}

module Data.Geodetic.GreatCircle where


import Data.Geodetic.Coordinate
import Data.Geodetic.GeodeticModel

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Typeable
import Control.Lens ((^.))
import Data.Fixed (mod')


data CourseDirection = CourseEast | CourseWest
                     deriving (Show, Eq, Typeable)
               

-- | greate circle distance
gcDist :: (GeodeticModel m, Floating t) => GeodeticCoordinate m t -> GeodeticCoordinate m t -> Length t
gcDist c1 c2 = c1 `gcDist'` c2
    where gcDist' (GeodeticCoordinate _ φ1 λ1 _) (GeodeticCoordinate _ φ2 λ2 _) =
            let dλ = abs $ λ1 - λ2
                a1 = (cos φ1 * sin dλ)
                a2 = (cos φ2 * sin φ1) - (sin φ2 * cos φ1 * cos dλ)
                a = sqrt ((a1 * a1) + (a2 *a2))     
                b = (sin φ2 * sin φ1) + (cos φ2 * cos φ1 * cos dλ)      
                dσ = atan (a / b)
                r = 6371.01 *~ kilo meter
            in r * dσ

-- | greate circle course
gcCourse :: (Ord t, Real t, Floating t, GeodeticModel a) =>
            CourseDirection -> GeodeticCoordinate a t -> GeodeticCoordinate a t -> PlaneAngle t
gcCourse d a b =
  let jb = b ^. longitude
      dg = gcDist a b / (1 *~ meter)
      ja = a ^. longitude 
      ar = ((sin jb) - (cos dg) * (sin ja)) / ((cos ja) * (sin dg))
      d360 = (360 *~ degree)
      c = case d of
        CourseWest -> ar
        CourseEast -> d360 - ar
  in ((c /~ degree) `mod'` 360) *~ degree
  
