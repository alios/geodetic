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

module Data.Geodetic.Helper where

import Numeric.Units.Dimensional.TF.Prelude

between :: (Ord a, Floating a) => a -> a -> Quantity DPlaneAngle a -> Bool
between a b l = l >= (a *~ degree)  && l <  (b *~ degree)

_10,_15,_16,_18,_21,_24,_27,_28,_32,_35,_45,_55,_58,_61,_64 :: (Num t) => Dimensionless t
_72,_90,_96,_120,_151,_252,_256,_298,_330, _600,_720,_1024,_3072 :: (Num t) => Dimensionless t
_10 = 10 *~ one 
_15 = 15 *~ one
_16 = 16 *~ one  
_18 = 18 *~ one
_21 = 21 *~ one
_24 = 24 *~ one
_28 = 28 *~ one
_27 = 27 *~ one 
_32 = 32 *~ one
_35 = 35 *~ one
_45 = 45 *~ one
_55 = 55 *~ one
_58 = 58 *~ one
_61 = 61 *~ one
_64 = 64 *~ one
_72 = 72 *~ one
_90 = 90 *~ one
_96 = 96 *~ one
_120 = 120 *~ one
_151 = 151 *~ one
_252 = 252 *~ one
_256 = 256 *~ one
_298 = 298 *~ one
_330 = 330 *~ one 
_600 = 600 *~ one
_720 = 720 *~ one 
_1024 = 1024 *~ one
_3072 = 3072 *~ one

