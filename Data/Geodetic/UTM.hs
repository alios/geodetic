{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Geodetic.UTM  where

import Data.Geodetic.Helper
import Data.Geodetic.Coordinate
import Data.Geodetic.GeodeticModel

import Control.Lens hiding (_1, _2, _3, _4, _5, _6, _8, _9, (*~))
import qualified Prelude as P
import Data.Typeable
import Numeric.Units.Dimensional.TF.Prelude
import Data.List (find)
import Data.Fixed (mod')

data UTM t = UTM {
  _utmZone :: (Int, Char),
  _utmEasting :: Length t,
  _utmNorthing :: Length t
  } deriving (Show, Eq, Typeable)
makeLenses ''UTM

toUTM :: (GeodeticModel m, RealFrac t, Floating t, Enum t, Show t, Show m) =>
         GeodeticCoordinate m t -> UTM t
toUTM coord =
  let a = semiMajorAxis $ coord ^. refElipsoid
      eccSquared = flattening $ coord ^. refElipsoid
      es = eccSquared
      es2 = es * es
      es3 = es2 * es
      eccPrimSquared = recProcFlattening $ coord ^. refElipsoid
      lat = coord ^. latitude
      long = let l' = (((coord ^. longitude) /~ degree) `mod'` 360) *~ degree
             in (l') - (180 *~ degree)
      k0 = 0.9996 *~ one
      zoneNumberI =
        case (isSpecialZone coord) of
          Nothing -> floor $ (((coord ^. longitude + (180 *~ degree)) / (6 *~ degree)) + _1) /~ one
          Just z -> z
      zoneNumber = (fromIntegral zoneNumberI) *~ one
      longOrigin = (zoneNumber - _1) * (6.0 *~ degree) - (180 *~ degree) + (3 *~ degree)
      zoneLetter = maybe (error $ "latitude out of length for UTM of: " ++ show coord) id $
                   utmLetter coord
      nn = a / sqrt(_1 - eccSquared * (sin lat) * (sin lat))
      tt = tan lat * tan lat
      tt2 = tt * tt 
      cc = eccPrimSquared * cos lat * cos lat
      cc2 = cc * cc
      aa = cos lat * (long - longOrigin)
      aa2 = aa * aa
      aa3 = aa2 * aa
      aa4 = aa3 * aa 
      aa5 = aa3 * aa * aa
      aa6 = aa5 * aa
      mm = a * (((_1 - (   es/_4) -  (_3 *es2/ _64) - ( _5*es3/ _256)) * lat           ) -
                ((     (_3*es/_8) +  (_3 *es2/ _32) + (_45*es3/_1024)) * sin (_2 * lat)) +
                ((                   (_15*es2/_256) + (_45*es3/_1024)) * sin (_4 * lat)) -
                ((                                    (_35*es3/_3072)) * sin (_6 * lat))
               )
      easting = (k0 * nn *
                 (aa + (_1-tt+cc) * aa3 / _6 +
                  (_5 - _18 * tt + tt2 + _72 * cc - _58 * eccPrimSquared) * aa5 / _120) +
                 500 *~ kilo meter)
      northing' = (k0 * (mm + nn * tan (lat) *
                         (aa2/_2 + (_5 - tt + _9 * cc + _4 * cc2) * aa4 / _24 +
                          (_61 - _58 * tt + tt2 + _600 * cc - _330 * eccPrimSquared) * aa6 / _720)))
      northing = if ( lat < 0 *~ degree) then northing' + 10000 *~ kilo meter else northing' 
  in UTM {
    _utmZone = (zoneNumberI, zoneLetter),
    _utmEasting = easting,
    _utmNorthing = northing
    }

utmLetter :: (Floating t, Eq t, Ord t, Enum t) => GeodeticCoordinate m t -> Maybe Char
utmLetter coord =
  let l = (coord ^. latitude) /~ degree 
  in if (l == 80)
     then Just 'X'
     else fmap fst $ find (\(_, (a,b)) -> (l >= a) && (l < b)) zs
  where cs = ['C' .. 'H'] ++ ['J'.. 'N'] ++ ['P'..'X']
        zs = zip cs $ fmap (\x -> (x, (P.+) x 8)) [ (P.+)(-80) ((P.*) x 8) | x <- [0 .. ]]

specZones1 :: (Ord t, Floating t) => GeodeticCoordinate m t -> Maybe Int
specZones1 coord =
  let lat = coord ^. latitude
      long = coord ^. longitude
      zs = fmap snd $ find (\(p, _) -> p long) ps
      ps = [ (between  0  9, 31)
           , (between  9 21, 33)
           , (between 21 33, 35)
           , (between 33 42, 37)
          ]
  in if (between 72 84 lat)
        then zs else Nothing

specZones2 :: (Ord t, Floating t) => GeodeticCoordinate m t -> Maybe Int
specZones2 coord =
  let lat = coord ^. latitude
      long = coord ^. longitude
  in if (between 56 64 lat && between 3 12 long)
     then Just 32 else Nothing

isSpecialZone :: (Ord t, Floating t) => GeodeticCoordinate m t -> Maybe Int
isSpecialZone coord =
  let z1 = specZones1 coord
      z2 = specZones2 coord
  in case (z1,z2) of
    (Nothing, Nothing) -> Nothing
    (Just z, Nothing) -> Just z
    (Nothing, Just z) -> Just z
    (Just _, Just _) -> error "isSpecZone found zone in spec zone 1 & 2"
  

