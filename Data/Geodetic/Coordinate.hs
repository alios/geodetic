{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geodetic.Coordinate where

import Control.Lens
import Data.Typeable
import Numeric.Units.Dimensional.TF.Prelude

data GeodeticCoordinate m t =
  GeodeticCoordinate {
    _refElipsoid :: !m,
    _latitude :: !(PlaneAngle t),
    _longitude :: !(PlaneAngle t),
    _height :: !(Length t)
    } deriving (Show, Eq, Typeable)
makeLenses ''GeodeticCoordinate

data ECEF t =
  ECEF {
    _coordX :: !(Length t),
    _coordY :: !(Length t),
    _coordZ :: !(Length t)
    } deriving (Show, Eq, Typeable)
makeLenses ''ECEF

