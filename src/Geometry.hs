-- Henderson's Functional Geometry functions: https://eprints.soton.ac.uk/257577/1/funcgeo2.pdf

module Geometry
  ( rot
  , flip
  , beside, besideS
  , above, aboveS
  , rot45
  , over
  , natrec
  ) where

import Prelude hiding (flip)
import Image

flip :: Image -> Image
flip = transformX $ \x -> 1.0 - x

over :: Image -> Image -> Image
over = overlay

besideS :: Int -> Int -> Image -> Image -> Image
besideS lr rr l r = sl `over` sr
 where 
  lr' = fromIntegral lr / fromIntegral (lr+rr) 
  rr' = fromIntegral rr / fromIntegral (lr+rr)
  sl  = transformX (\x -> x * lr')       l
  sr  = transformX (\x -> x * rr' + lr') r

beside :: Image -> Image -> Image
beside = besideS 1 1

aboveS :: Int -> Int -> Image -> Image -> Image
aboveS tr br t b = st `over` sb
 where 
  tr' = fromIntegral tr / fromIntegral (tr+br) 
  br' = fromIntegral br / fromIntegral (tr+br)
  st  = transformY (\y -> y * tr') t
  sb  = transformY (\y -> y * br' + tr') b

above :: Image -> Image -> Image
above = aboveS 1 1 

rot :: Image -> Image
rot = transform $ \(x, y) -> (y, 1.0 - x)

rot45 :: Image -> Image
rot45 = transform $ \(x, y) -> ((x + y) / 2, (y - x) / 2)

natrec :: Image -> (Int -> Image -> Image) -> Int -> Image
natrec b f 0 = b
natrec b f n = f (n-1) (natrec b f (n-1))
