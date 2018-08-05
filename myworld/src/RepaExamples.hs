module RepaExamples where

import Codec.Picture.Types
import Codec.Picture
import Data.Complex

import RGBMap
import Map
import PlanarCoordinate
import Resolution

data XYR = XYR { xyr_x :: Double, xyr_y :: Double, xyr_r :: Double }
xyrToSector :: XYR -> Sector
xyrToSector (XYR x y r) = Sector (Planar (x - r) (y + r)) (Planar (x + r) (y - r))

mandelbrot :: Complex Double -> Complex Double -> Int -> Bool
mandelbrot z _ _ | (sqr $ realPart z) + (sqr $ imagPart z) > 4 = False where sqr a = a * a
mandelbrot _ _ 0 = True
mandelbrot z c i = mandelbrot (z*z + c) c (i - 1)

mandelmap' :: Int -> XYR -> Resolution -> RGBMap
mandelmap' n xyr r =
  let mandSector = xyrToSector xyr
      imgSector  = resToSector r
      xform      = mapCoordinates imgSector mandSector
  in  MapT $ \p -> return $
    let (Planar x y) = runTransform xform p
        z            = x :+ y
    in if mandelbrot z z n then black' else white'

black' :: RGB8
black' = (0, 0, 0)

white' :: RGB8
white' = (255, 255, 255)

saveMandelimg :: String -> Int -> XYR -> Resolution -> IO ()
saveMandelimg fp n xyr r = savePngImage fp . ImageRGB8 $ fromRGBMap (mandelmap' n xyr r) r
