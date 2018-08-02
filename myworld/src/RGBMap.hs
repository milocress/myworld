module RGBMap where

import Codec.Picture.Types
import Data.Array.Repa
import Data.Functor.Identity

import Map
import PlanarCoordinate
import Resolution

type RGB8   = (Pixel8, Pixel8, Pixel8)
type RGBMap = Map RGB8

toPixel :: RGB8 -> PixelRGB8
toPixel (r, g, b) = PixelRGB8 r g b

fromRGBMap :: RGBMap -> Resolution -> Image PixelRGB8
fromRGBMap  m r = fromArrToImg . unboxArr $ fromMapToArr m r

unboxArr :: Array D DIM2 RGB8 -> Array U DIM2 RGB8
unboxArr = runIdentity . computeUnboxedP

fromMapToArr :: RGBMap -> Resolution -> Array D DIM2 RGB8
fromMapToArr m (Resolution w h) = fromFunction (Z :. w :. h) $ \(Z :. x :. y) ->
  runMap m $ Planar (fromIntegral x) (fromIntegral y)

fromArrToImg :: Array U DIM2 RGB8 -> Image PixelRGB8
fromArrToImg a =
  let (Z :. w :. h) = extent a
  in generateImage (\x y -> toPixel $ a ! (Z :. x :. y)) w h

resToSector :: Resolution -> Sector
resToSector (Resolution x y) = Sector
                               (Planar 0 (fromIntegral y))
                               (Planar (fromIntegral x) 0)
