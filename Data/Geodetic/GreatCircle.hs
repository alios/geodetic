{-# LANGUAGE DeriveDataTypeable #-}

{-
Copyright (c) 2014, Markus Barenhoff <alios@alios.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Data.Geodetic.GreatCircle where


import Data.Geodetic.Coordinate
import Data.Geodetic.GeodeticModel

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Typeable
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
  let jb = _longitude b
      dg = gcDist a b / (1 *~ meter)
      ja = _longitude a
      ar = ((sin jb) - (cos dg) * (sin ja)) / ((cos ja) * (sin dg))
      d360 = (360 *~ degree)
      c = case d of
        CourseWest -> ar
        CourseEast -> d360 - ar
  in ((c /~ degree) `mod'` 360) *~ degree
  
