module PixelMap where

import Codec.Picture.Types
import Data.Functor.Identity

import Map
import PlanarCoordinate
import Resolution

type PixelMap = Map PixelRGB8

fromPixelMap :: PixelMap -> Resolution -> Image PixelRGB8
fromPixelMap m (Resolution w h) = generateImage (\x y -> runMap m $ Planar (fromIntegral x) (fromIntegral y)) w h
-- f is a constructor for PlanarCoordinate.
