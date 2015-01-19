{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

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

module Data.Geodetic.Elipsoids where

import Data.Geodetic.GeodeticModel

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude

data WGS84 = ElipsoidWGS84

instance (Floating t, Show t) => GeodeticModel WGS84 t where
  data GeodeticCoordinate WGS84 t =
    WGS84 { _wgs84Lat :: PlaneAngle t
          , _wgs84Long :: PlaneAngle t
          , _wgs84Height :: Length t
          } deriving (Show)
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257223563 *~ one)
  refElipsoid _ = ElipsoidWGS84
  mkCoordinate = WGS84
  latitude = _wgs84Lat
  longitude = _wgs84Long
  height = _wgs84Height

data ANS = ElipsoidANS

instance (Floating t, Show t) => GeodeticModel ANS t where
  data GeodeticCoordinate ANS t =
    ANS { _ansLat :: PlaneAngle t
          , _ansLong :: PlaneAngle t
          , _ansHeight :: Length t
          } deriving (Show)
  semiMajorAxis _ = (6378160.0 *~ meter)
  recProcFlattening _ = (298.25 *~ one)
  refElipsoid _ = ElipsoidANS
  mkCoordinate = ANS
  latitude = _ansLat
  longitude = _ansLong
  height = _ansHeight

data GRS80 = ElipsoidGRS80

instance (Floating t, Show t) => GeodeticModel GRS80 t where
  data GeodeticCoordinate GRS80 t =
    GRS80 { _grs80Lat :: PlaneAngle t
          , _grs80Long :: PlaneAngle t
          , _grs80Height :: Length t
          } deriving (Show)
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257222101 *~ one)
  refElipsoid _ = ElipsoidGRS80
  mkCoordinate = GRS80
  latitude = _grs80Lat
  longitude = _grs80Long
  height = _grs80Height

