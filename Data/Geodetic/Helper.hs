
module Data.Geodetic.Helper where

import Numeric.Units.Dimensional.TF.Prelude

between :: (Ord a, Floating a) => a -> a -> Quantity DPlaneAngle a -> Bool
between a b l = l >= (a *~ degree)  && l <  (b *~ degree)

_15,_18,_24,_32,_35,_45,_58,_61,_64,_72,_120,_256,_330, _600,_720,_1024,_3072 :: (Num t) => Dimensionless t
_15 = 15 *~ one  
_18 = 18 *~ one
_24 = 24 *~ one 
_32 = 32 *~ one
_35 = 35 *~ one
_45 = 45 *~ one
_58 = 58 *~ one
_61 = 61 *~ one
_64 = 64 *~ one
_72 = 72 *~ one
_120 = 120 *~ one 
_256 = 256 *~ one
_330 = 330 *~ one 
_600 = 600 *~ one
_720 = 720 *~ one 
_1024 = 1024 *~ one
_3072 = 3072 *~ one

