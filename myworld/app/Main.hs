module Main where

import Codec.Picture
import System.Environment

import PlanarCoordinate
import RGBMap
import RepaExamples
import Resolution

file :: String
file = "./map.png"

main :: IO ()
-- main = savePngImage file . ImageRGB8 $ fromPixelMap m_mand 1920 1080
main = do
  args <- getArgs
  let x = read $ args !! 0
      y = read $ args !! 1
      r = read $ args !! 2
      n = read $ args !! 3
      w = read $ args !! 4
      h = read $ args !! 5
      f = args !! 6
    in saveMandelimg f n (XYR x y r) (Resolution w h)
-- X = -0.16
-- Y = 1.0405
-- R = 0.026
