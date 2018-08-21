module MandelMain where

import Codec.Picture
import System.Environment

import PlanarCoordinate
import RGBMap
import RepaExamples
import Resolution
import SectorMapExamples
import ShapeMapExamples

file :: String
file = "./map.png"

mandelMain :: IO ()
mandelMain = do
  args <- getArgs
  let x = read $ args !! 0
      y = read $ args !! 1
      r = read $ args !! 2
      n = read $ args !! 3
      w = read $ args !! 4
      h = read $ args !! 5
      f = args !! 6
    in savePngImage f $ lowPolyMandelImg n (XYR x y r) (Resolution w h)
-- X = -0.16
-- Y = 1.0405
-- R = 0.026
