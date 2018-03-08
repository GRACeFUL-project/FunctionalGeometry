-- Henderson's Functional Geometry functions: https://eprints.soton.ac.uk/257577/1/funcgeo2.pdf

module Geometry
  ( rot
  , flip
  , beside, besideS
  , above, aboveS
  , rot45
  , over
  ) where

import Prelude hiding (flip)
import Image

flip :: Image -> Image
flip = transformX $ \x -> 1.0 - x

over :: Image -> Image -> Image
over = overlay

besideS :: Float -> Float -> Image -> Image -> Image
besideS lr rr l r = sl `over` sr
 where 
  sl = transformX (\x -> x * lr)      l
  sr = transformX (\x -> x * rr + lr) r

beside :: Image -> Image -> Image
beside = besideS 0.5 0.5

aboveS :: Float -> Float -> Image -> Image -> Image
aboveS tr br t b = st `over` sb
 where 
  st = transformY (\y -> y * tr) t
  sb = transformY (\y -> y * br + tr) b

above :: Image -> Image -> Image
above = aboveS 0.5 0.5

rot :: Image -> Image
rot = transform $ \(x, y) -> (y, 1.0 - x)

rot45 :: Image -> Image
rot45 = transform $ \(x, y) -> ((x + y) / 2, (y - x) / 2)
