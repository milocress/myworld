module SectorMapExamples where

import Codec.Picture

import Map
import SectorMap
import RGBMap
import RepaExamples
import Resolution

boundedMandelMap :: Int -> XYR -> Resolution -> SectorMap RGB8
boundedMandelMap n xyr r = fromMap (mandelmap' n xyr r) $ resToSector (Resolution 1920 1080)

wierdLookingMap :: Int -> XYR -> Resolution -> RGBMap
wierdLookingMap n xyr r = gradient' >>> (boundedMandelMap n xyr r)

saveWierdLookingMap :: String -> Int -> XYR -> Resolution -> IO ()
saveWierdLookingMap fp n xyr r = savePngImage fp . ImageRGB8 $ fromRGBMap (wierdLookingMap n xyr r) r
