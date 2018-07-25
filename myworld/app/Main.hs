module Main where

import PixelMap
import Map
import Coordinate
import Codec.Picture.Types
import Codec.Picture
import Data.Complex

file :: String
file = "./map.png"

main :: IO ()
-- main = savePngImage file $ ImageRGB8 $ fromPixelMap m_mand Planar 1920 1080
main = savePngImage file . ImageRGB8 $ fromRGBMap m_mand' Planar 1920 1080

gradient :: PixelMap
gradient = MapT $ \(Planar x y) -> return $ PixelRGB8 (mod (floor x) 255) (mod (floor y) 255) 255

m_mand :: PixelMap
m_mand = mandelmap 1000 $ Transform $ \(Planar x y) -> Planar (x / 600 - 2) (y / 600 - (1080/1200))

mandelmap :: Int -> Transform -> PixelMap
mandelmap n xform = MapT $ \p -> return $
  let (Planar x y) = runTransform xform p
      z            = x :+ y
  in if mandelbrot z z n then black else white

mandelbrot :: Complex Double -> Complex Double -> Int -> Bool
mandelbrot z _ _ | (sqr $ realPart z) + (sqr $ imagPart z) > 4 = False where sqr a = a * a
mandelbrot _ _ i | i <= 0 = True
mandelbrot z c i = mandelbrot (z*z + c) c (i - 1)

black :: PixelRGB8
black = PixelRGB8 0 0 0

white :: PixelRGB8
white = PixelRGB8 255 255 255

mandelmap' :: Int -> Transform -> RGBMap
mandelmap' n xform = MapT $ \p -> return $
  let (Planar x y) = runTransform xform p
      z                = x :+ y
  in if mandelbrot z z n then black' else white'

black' :: RGB8
black' = (0, 0, 0)

white' :: RGB8
white' = (255, 255, 255)

m_mand' :: RGBMap
m_mand' = mandelmap' 1000 $ Transform $ \(Planar x y) -> Planar (x / 600 - 2) (y / 600 - (1080/1200))
