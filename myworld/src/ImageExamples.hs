module ImageExamples where

import PixelMap
import Map
import PlanarCoordinate
import Codec.Picture.Types
import Codec.Picture
import Data.Complex

gradient :: PixelMap
gradient = MapT $ \(x, y) -> return $ PixelRGB8 (mod (floor x) 255) (mod (floor y) 255) 255

m_mand :: PixelMap
m_mand = mandelmap 1000 $ Transform $ \(x, y) -> (x / 600 - 2, y / 600 - (1080/1200))

mandelmap :: Int -> Transform -> PixelMap
mandelmap n xform = MapT $ \p -> return $
  let (x, y) = runTransform xform p
      z            = x :+ y
  in if mandelbrot z z n then black else white
  -- Note that this is the OLD way of creating an image, and is not considered best practice. For modern
  -- image examples, refer to RepaExamples.
  -- This file is preserved mainly for legacy purposes, and the code contained here is slower and more
  -- naive than in other files.

mandelbrot :: Complex Double -> Complex Double -> Int -> Bool
mandelbrot z _ _ | (sqr $ realPart z) + (sqr $ imagPart z) > 4 = False where sqr a = a * a
mandelbrot _ _ i | i <= 0 = True
mandelbrot z c i = mandelbrot (z*z + c) c (i - 1)

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 255 255 255
