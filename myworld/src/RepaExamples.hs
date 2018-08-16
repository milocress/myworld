module RepaExamples where

import Codec.Picture.Types
import Codec.Picture
import Data.Complex

import RGBMap
import Map
import PlanarCoordinate
import Resolution

mandelbrot :: Complex Double -> Complex Double -> Int -> Bool
mandelbrot z _ _ | (sqr $ realPart z) + (sqr $ imagPart z) > 4 = False where sqr a = a * a
mandelbrot _ _ 0 = True
mandelbrot z c i = mandelbrot (z*z + c) c (i - 1)

mandelmap :: Int -> XYR -> Resolution -> RGBMap
mandelmap n = MapT $ \(x, y) -> return $
  let z   = x :+ y
  in if mandelbrot z z n then black' else white'

black' :: RGB8
black' = (0, 0, 0)

white' :: RGB8
white' = (255, 255, 255)

gradient :: RGBMap
gradient = MapT $ \(x, y) -> return $ ((mod (floor x) 255), (mod (floor y) 255), 255)
