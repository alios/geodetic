{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GeodeticSpec (spec) where

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Data.Geodetic
import Test.Hspec
import Test.QuickCheck


instance Arbitrary (GeodeticCoordinate WGS84 Double) where
  arbitrary = do
    lat <- arbitrary `suchThat` (\l -> (l >= -90) && (l < 90))
    lon <- arbitrary `suchThat` (\l -> (l >= -180) && (l < 180))
    h <- arbitrary   `suchThat` (\h -> (h >= -100) && (h < 100))
    return $ mkCoordinate (lat *~ degree) (lon *~ degree) (h *~ meter)



spec :: Spec
spec = describe "ECEF conversion" $ do
  it "should produce no latitude error" $
    property $ propEcefLat 
  it "should not produce longitude error larger then 0.01 degree" $
    property $ propEcefLong
  it "should not produce height error larger then 0.01 meter" $
    property $ propEcefHeight 


la = 5.7427358164264024e-2 *~ degree
lo = (-2.866380396973443e-2) *~ degree
lh = (-1.6819506351663547) *~ meter
aa :: GeodeticCoordinate WGS84 Double
aa = WGS84 {_wgs84Lat = la, _wgs84Long = lo, _wgs84Height = lh}
ad = ecefDiff aa
pla = propEcefLat aa
plb = propEcefLong aa
plc = propEcefHeight aa

propEcefLat, propEcefLong, propEcefHeight :: (GeodeticModel WGS84 Double) =>
                GeodeticCoordinate WGS84 Double -> Bool
propEcefLat c =
  let d = ecefDiff c
  in ((abs $ latitude d)) == (0 *~ degree)

propEcefLong c =
  let d = ecefDiff c
  in ((abs $ longitude d)) <= (0.01 *~ degree)

propEcefHeight c =
  let d = ecefDiff c
  in ((abs $ height d)) <= (0.01 *~ meter)

ecefDiff :: (RealFloat t, GeodeticModel m t) =>
            GeodeticCoordinate m t -> GeodeticCoordinate m t
ecefDiff a =
  let b = toEcef a
      c = fromEcef (refElipsoid a) b
  in mkCoordinate (latitude a - latitude c)
                  (longitude a - longitude c)
                  (height a - height c)





propUTM :: (GeodeticModel m t, Enum t, RealFloat t) =>
            GeodeticCoordinate m t -> Bool
propUTM a =
  let r = ecefDiff a
  in ((abs $ latitude r) <= maxLatErr) &&
     ((abs $ longitude r) <= maxLongErr) &&
     (height r == 0 *~ meter)
  where maxLatErr = 0.08 *~ degree
        maxLongErr = 0.08*~ degree
        

propUTMLong c =
  let d = utmDiff c
  in ((abs $ longitude d)) <= (0.01 *~ degree)

propUTMLat c =
  let d = utmDiff c
  in ((abs $ latitude d)) <= (0.01 *~ degree)

propUTMHeight c =
  let d = utmDiff c
  in ((abs $ height d)) == 0 *~ meter


utmDiff :: (Enum t, RealFloat t, GeodeticModel m t) =>
            GeodeticCoordinate m t -> GeodeticCoordinate m t
utmDiff a =
  let b = toUTM a
      c = fromUTM (refElipsoid a) b
  in mkCoordinate (latitude a - latitude c)
                  (longitude a - longitude c)
                  (height a - height c)
