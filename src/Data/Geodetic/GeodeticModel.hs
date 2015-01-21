{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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

module Data.Geodetic.GeodeticModel where

import qualified Prelude ()
import Data.Geodetic.Coordinate
import Numeric.Units.Dimensional.TF.Prelude
data Hemisphere = Northern | Soutern
  

class (Ord t, Floating t, Show t, Show m, Show (GeodeticCoordinate m t)) =>
      GeodeticModel m t where
  data GeodeticCoordinate m t :: *
  refElipsoid :: GeodeticCoordinate m t -> m
  latitude  :: GeodeticCoordinate m t -> PlaneAngle t
  longitude :: GeodeticCoordinate m t -> PlaneAngle t
  height :: GeodeticCoordinate m t -> Length t
  mkCoordinate :: PlaneAngle t -> PlaneAngle t -> Length t ->
                  GeodeticCoordinate m t
                  
  semiMajorAxis :: (Fractional t) => m -> Length t
  recProcFlattening :: (Fractional t) => m -> Dimensionless t
  flattening :: (Fractional t) => m -> Dimensionless t
  flattening m = _1 / (recProcFlattening m)
  semiMinorAxis :: (Fractional t) => m -> Length t
  semiMinorAxis m = (semiMajorAxis m) * (_1 - flattening m)
  fstEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  fstEccentricity m =
    let f = flattening m
    in (_2 * f) - (f ** _2)
  sndEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  sndEccentricity m =     
    let f = flattening m
        a = f * (_2 - f)  
        b = ((_1 - f) ** _2)
    in  a / b
  toEcef :: (Floating t) => GeodeticCoordinate m t -> ECEF t
  toEcef c =
    let φ = longitude c
        λ = latitude c
        h = height c
        e2 = fstEccentricity $ refElipsoid c
        x = sqrt (_1 - (e2 * ((sin φ) ** _2)))
        a = semiMajorAxis $ refElipsoid c
        normal = a / x
        normalh = normal + h
        rx = normalh * (cos φ) * (cos λ)
        ry = normalh * (cos φ) * (sin λ)
        rz = (((a * ( _1 - e2)) / x) + h) * (sin φ)
    in ECEF rx ry rz
  fromEcef :: (RealFloat t) => m -> ECEF t -> GeodeticCoordinate m t
  fromEcef m coord = 
    let x = _coordX coord 
        y = _coordY coord 
        z = _coordZ coord
        a = semiMajorAxis m
        b = semiMinorAxis m
        e2 = fstEccentricity m
        e'2 = sndEccentricity m
        r = sqrt ((x * x) + (y * y))
        ee2 = (a * a) - (b * b)
        f = (54 *~ one) * (b * b) * (z * z)
        g = (r * r) + ((_1 - e2) * z * z) - (e2 * e2 * ee2)
        c = (e2 * e2 * f * r * r ) / ( g * g * g )
        s = cbrt (_1 + c + sqrt ((c*c) + (_2 * c)))  
        p = f / (_3 * ((s + (_1 / s) + _1) ** _2) * (g * g))
        q = sqrt (_1 + (_2 * e2 * e2 * p))
        r0a = ((_0 - _1) * (p * e2 * r)) / (_1 + q) 
        r0b = sqrt ((((_1 / _2) * a * a) * (_1 + (_1 / q)))- 
                    ((p * (_1 - e2) * z * z)/(q * (_1 + q))) - 
                    ((_1 / _2) * p * r * r))
        r0 = r0a + r0b
        ub = z * z
        ua = (r  - (e2 * r0)) * (r  - (e2 * r0))
        u = sqrt (ua + ub) 
        v = sqrt (ua + ((_1 - e2) * ub))
        z0 = (b * b * z) / (a * v)
        h = u * (_1 - ((b * b)/(a * v)))
        φ = atan ((z + (e'2 * z0)) / r)
        λ = atan2 y x
    in mkCoordinate λ φ h
