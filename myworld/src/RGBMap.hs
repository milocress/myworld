module RGBMap where

import Codec.Picture.Types
import Data.Array.Repa
import Data.Functor.Identity

import Map
import Transform
import Resolution
import PlanarCoordinate

type RGB8   = (Pixel8, Pixel8, Pixel8)
type RGBMap = Map RGB8

toPixel :: RGB8 -> PixelRGB8
toPixel (r, g, b) = PixelRGB8 r g b

toImg :: RGBMap -> XYR -> Resolution -> DynamicImage
toImg m xyr r = fromRGBMap m' r where
  xform = mapCoordinates (resToSector r) (xyrToSector xyr)
  m'    = transform m xform


fromRGBMap :: RGBMap -> Resolution -> DynamicImage
fromRGBMap m r = ImageRGB8 . fromArrToImg . unboxArr $ fromMapToArr m r

unboxArr :: Array D DIM2 RGB8 -> Array U DIM2 RGB8
unboxArr = runIdentity . computeUnboxedP

fromMapToArr :: RGBMap -> Resolution -> Array D DIM2 RGB8
fromMapToArr m (Resolution w h) = fromFunction (Z :. w :. h) $ \(Z :. x :. y) ->
  runMap m $ (fromIntegral x, fromIntegral y)

fromArrToImg :: Array U DIM2 RGB8 -> Image PixelRGB8
fromArrToImg a =
  let (Z :. w :. h) = extent a
  in generateImage (\x y -> toPixel $ a ! (Z :. x :. y)) w h
