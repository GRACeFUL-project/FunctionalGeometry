-- Images represented as a list of cubic bezier lines

module Image
  ( Image(..)
  , blank, addCubicBezier
  , Point
  , transform, transformX, transformY, scale
  , overlay
  , writeImage, encodeImage
  ) where

import Codec.Picture (PixelRGBA8(..), writePng, encodePng)
import qualified Codec.Picture as JP
import Graphics.Rasterific hiding (Point, transform)
import Graphics.Rasterific.Texture
import Data.ByteString.Base64.Lazy (encode)
import Data.ByteString.Lazy (ByteString)

-- Types
newtype Image = Image [CubicBezier] deriving Show  -- an image is an list of cubic bezier lines
type    Point = (Float, Float)                     -- making life easier with tuples

-- Construct images
blank :: Image
blank = Image []

addCubicBezier :: Point -> Point -> Point -> Point -> Image -> Image
addCubicBezier a b c d (Image cbs) = Image (cb:cbs)
 where
  cb = CubicBezier (fromPoint a) (fromPoint b) (fromPoint c) (fromPoint d)

overlay :: Image -> Image -> Image
overlay (Image xs) (Image ys) = Image (xs ++ ys)

-- Change images
transform :: (Point -> Point) -> Image -> Image
transform f (Image cbs) = Image
  [CubicBezier (f' a)  (f' b) (f' c) (f' d) | CubicBezier a b c d <- cbs]
 where
  f' = liftPoint f

transformX :: (Float -> Float) -> Image -> Image
transformX f = transform $ \(x, y) -> (f x, y)

transformY :: (Float -> Float) -> Image -> Image
transformY f = transform $ \(x, y) -> (x, f y)

scale :: Float -> Image -> Image
scale n = transform (\(x, y) -> (x*n, y*n))

-- Write image to file
writeImage :: FilePath -> Image -> IO ()
writeImage file = writePng file . render

encodeImage :: Image -> ByteString
encodeImage = encode . encodePng . render

render :: Image -> JP.Image PixelRGBA8
render (Image cbs) = renderDrawing 1000 1000 white texture
 where 
  white   = PixelRGBA8 255 255 255 255
  black   = PixelRGBA8   0   0   0 255
  texture = withTexture (uniformTexture black) drawing
  drawing = mconcat $ map (\b -> stroke 1 JoinRound (CapRound, CapRound) b) cbs

-- Point conversions
fromPoint :: Point -> V2 Float
fromPoint (x, y) = V2 x y

toPoint :: V2 Float -> Point
toPoint (V2 x y) = (x, y)

liftPoint :: (Point -> Point) -> V2 Float -> V2 Float
liftPoint f = fromPoint . f . toPoint
