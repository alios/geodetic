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

module Data.Geodetic.Elipsoids where

import Data.Geodetic.GeodeticModel
import Data.Geodetic.Coordinate

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Typeable


data ANS = ANS deriving (Show, Eq, Typeable)
instance GeodeticModel ANS where
  geodeticModel = ANS
  semiMajorAxis _ = (6378160.0 *~ meter)
  recProcFlattening _ = (298.25 *~ one)

ans' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate ANS t
ans' φ λ h = GeodeticCoordinate ANS φ λ h

ans :: (Floating t) => t -> t -> t -> GeodeticCoordinate ANS t
ans φ λ h = ans' (φ *~ degree) (λ *~ degree) (h *~ meter)


data WGS84 = WGS84 deriving (Show, Eq, Typeable)
instance GeodeticModel WGS84 where
  geodeticModel = WGS84
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257223563 *~ one)

wgs84' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate WGS84 t
wgs84' φ λ h = GeodeticCoordinate WGS84 φ λ h

wgs84 :: (Floating t) => t -> t -> t -> GeodeticCoordinate WGS84 t
wgs84 φ λ h = wgs84'(φ *~ degree) (λ *~ degree) (h *~ meter)


data GRS80 = GRS80 deriving (Show, Eq, Typeable)
instance GeodeticModel GRS80  where
  geodeticModel = GRS80
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257222101 *~ one)

grs80' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate GRS80 t
grs80' φ λ h = GeodeticCoordinate GRS80 φ λ h

grs80 :: (Floating t) => t -> t -> t -> GeodeticCoordinate GRS80 t
grs80 φ λ h = grs80'(φ *~ degree) (λ *~ degree) (h *~ meter)
