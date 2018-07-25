module PixelMap where

import Codec.Picture.Types
import Data.Array.Repa
import Map
import Coordinate
import Data.Functor.Identity

type RGB8     = (Pixel8, Pixel8, Pixel8)
type PixelMap = Map PixelRGB8

type RGBMap   = Map RGB8

toPixel :: RGB8 -> PixelRGB8
toPixel (r, g, b) = PixelRGB8 r g b

fromPixelMap :: PixelMap -> (Double -> Double -> Coordinate) -> Int -> Int -> Image PixelRGB8
fromPixelMap m f = generateImage $ \x y -> runMap m $ f (fromIntegral x) (fromIntegral y)
-- f is a constructor for Coordinate.

fromRGBMap :: RGBMap -> (Double -> Double -> Coordinate) -> Int -> Int -> Image PixelRGB8
fromRGBMap  m f w h = fromArrToImg . unboxArr $ fromMapToArr m f w h

unboxArr :: Array D DIM2 RGB8 -> Array U DIM2 RGB8
unboxArr = runIdentity . computeUnboxedP

fromMapToArr :: RGBMap -> (Double -> Double -> Coordinate) -> Int -> Int -> Array D DIM2 RGB8
fromMapToArr m f w h = fromFunction (Z :. w :. h) $ \(Z :. x :. y) ->
  runMap m $ f (fromIntegral x) (fromIntegral y)

fromArrToImg :: Array U DIM2 RGB8 -> Image PixelRGB8
fromArrToImg a =
  let (Z :. w :. h) = extent a
  in generateImage (\x y -> toPixel $ a ! (Z :. x :. y)) w h
