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

module Data.Geodetic.UTM  where

import Data.Geodetic.Helper
import Data.Geodetic.Coordinate
import Data.Geodetic.GeodeticModel
import Data.Geodetic.Elipsoids

import qualified Prelude as P
import Data.Typeable
import Numeric.Units.Dimensional.TF.Prelude
import Data.List (find)

data UTM m t = UTM {
  _utmZone :: (Int, Char),
  _utmEasting :: Length t,
  _utmNorthing :: Length t,
  _utmHeight :: Length t
  } deriving (Show, Eq, Typeable)


k0 :: (Floating t) => Dimensionless t
k0 = 0.9996 *~ one

eastingOffset :: (Num t) => Length t
eastingOffset = 500 *~ kilo meter

southernOffset :: (Num t) => Length t
southernOffset = 10000 *~ kilo meter




toUTM :: (GeodeticModel m t, RealFrac t, Enum t) =>
         GeodeticCoordinate m t -> UTM m t
toUTM coord =
  let a = semiMajorAxis $ refElipsoid coord 
      eccSquared = fstEccentricity $ refElipsoid coord
      eccPrimSquared = eccSquared / (_1 - eccSquared)
      es = eccSquared
      es2 = es * es
      es3 = es2 * es
      lat = latitude coord
      long = longitude coord
      zoneNumberI =
        case (isSpecialZone coord) of
          Nothing -> floor $ (((long + (180 *~ degree)) / (6 *~ degree)) + _1) /~ one
          Just z -> z
      zoneNumber = (fromIntegral zoneNumberI) *~ one
      longOrigin = (zoneNumber - _1) * (6 *~ degree) - (180 *~ degree) + (3 *~ degree)
      zoneLetter = maybe (error $ "latitude out of length for UTM of: " ++ show coord) id $
                   utmLetter coord
      nn = a / sqrt(_1 - es * (sin lat) * (sin lat))
      tt = tan lat * tan lat
      tt2 = tt * tt 
      cc = eccPrimSquared * (cos lat) * (cos lat)
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
                  (_5 - _18 * tt + tt2 + _72 * cc - _58 * eccPrimSquared) * aa5 / _120) + eastingOffset)
      northing' = (k0 * (mm + nn * tan (lat) *
                         (aa2/_2 + (_5 - tt + _9 * cc + _4 * cc2) * aa4 / _24 +
                          (_61 - _58 * tt + tt2 + _600 * cc - _330 * eccPrimSquared) * aa6 / _720)))
      northing = if ( lat < 0 *~ degree) then northing' + southernOffset else northing' 
  in UTM {
    _utmZone = (zoneNumberI, zoneLetter),
    _utmEasting = easting,
    _utmNorthing = northing,
    _utmHeight = height coord
    }


fromUTM :: (GeodeticModel m t) => m -> UTM m t -> GeodeticCoordinate m t 
fromUTM m utm =
  let x = (_utmEasting utm) - eastingOffset
      y = (_utmNorthing utm) - if isNorthern
                               then (0 *~ meter) else southernOffset
      a = semiMajorAxis $ m
      (zoneNumberI, zoneLetter) = (_utmZone utm)
      zoneNumber = (fromIntegral zoneNumberI) *~ one
      isNorthern = zoneLetter >= 'N'
      longOrigin = (zoneNumber - _1) * (6 *~ degree) -
                   (180 *~ degree) + (3 *~ degree)
      eccPrimSquared = eccSquared / (_1 - eccSquared)
      eccSquared = fstEccentricity m
      es = eccSquared
      e1 = (_1 - sqrt(_1 - es)) / (_1 + sqrt(_1 - es))
      e12 = e1 * e1 
      e13 = e12 * e1
      e14 = e13 * e1
      es2 = es * es
      es3 = es2 * es
      mm = y / k0
      mu = mm / (a * (_1 - es/_4 - _3*es2/_64 - _5*es3/_256))
      phi1 = mu + (_3*e1/_2 - _27*e13/_32) * sin (_2 * mu) +
             (_21*e12/_16 - _55*e14/_32) * sin (_4 * mu) +
             (_151*e13/_96) * sin (_6 * mu)
      n1 = a / sqrt(_1 - es * sin(phi1) * sin(phi1))
      t1 = tan(phi1) * tan(phi1)
      c1 = eccPrimSquared * cos(phi1) * cos(phi1)
      r1 = a * (_1 - es) / (_1 - es * sin(phi1) * sin(phi1)) ** (1.5 *~ one)
      d = x / (n1*k0)
      d2 = d * d
      d3 = d2 * d
      d4 = d3 * d
      d5 = d4 * d
      d6 = d5 * d
      lat = phi1 - (n1 * tan(phi1) / r1) *
            (d2/_2 - (_5 + _3*t1 + _10*c1 - _4*c1*c1 - _9*eccPrimSquared) * d4/_24 +
            (_61 + _90*t1 + _298*c1 + _45*t1*t1 - _252*eccPrimSquared - _3*c1*c1) * d6 / _720)
      long' = (d - (_1 + _2*t1 + c1) * d3/_6 +
               (_5 - _2*c1 + _28*t1 - _3*c1*c1 + _8*eccPrimSquared + _24*t1*t1) *
               d5/_120) / cos(phi1)
      long = longOrigin - long'
  in mkCoordinate lat long (_utmHeight utm)


utmLetter :: (GeodeticModel m t, Ord t, Enum t) =>
             GeodeticCoordinate m t -> Maybe Char
utmLetter coord =
  let l = (latitude coord) /~ degree 
  in if (l == fromRational 80)
     then Just 'X'
     else fmap fst $ find (\(_, (a,b)) -> (l >= a) && (l < b)) zs
  where cs = ['C' .. 'H'] ++ ['J'.. 'N'] ++ ['P'..'X']
        zs = zip cs $ fmap (\x -> (x, (P.+) x 8))
             [ (P.+)(-80) ((P.*) x 8) | x <- [0 .. ]]


specZones1 :: (GeodeticModel m t, Ord t) =>
              GeodeticCoordinate m t -> Maybe Int
specZones1 coord =
  let lat = latitude coord
      long = longitude coord
      zs = fmap snd $ find (\(p, _) -> p long) ps
      ps = [ (between  0  9, 31)
           , (between  9 21, 33)
           , (between 21 33, 35)
           , (between 33 42, 37)
          ]
  in if (between 72 84 lat)
        then zs else Nothing

specZones2 :: (GeodeticModel m t, Ord t) =>
              GeodeticCoordinate m t -> Maybe Int
specZones2 coord =
  let lat = latitude coord
      long = longitude coord
  in if (between 56 64 lat && between 3 12 long)
     then Just 32 else Nothing


isSpecialZone :: (GeodeticModel m t, Ord t) =>
                 GeodeticCoordinate m t -> Maybe Int
isSpecialZone coord =
  let z1 = specZones1 coord
      z2 = specZones2 coord
  in case (z1,z2) of
    (Nothing, Nothing) -> Nothing
    (Just z, Nothing) -> Just z
    (Nothing, Just z) -> Just z
    (Just _, Just _) -> error "isSpecZone found zone in spec zone 1 & 2"
