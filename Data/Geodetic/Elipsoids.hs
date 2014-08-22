{-# LANGUAGE DeriveDataTypeable #-}

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
