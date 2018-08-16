module SectorMapExamples where

import Codec.Picture
import Control.Monad.Trans.Class

import Map
import SectorMap
import RGBMap
import RepaExamples
import Resolution
import Transform

wierdLookingImg :: Int -> XYR -> Resolution -> DynamicImage
wierdLookingImg n xyr r = fromRGBMap wierdLookingMap r where
  xform                 = mapCoordinates (resToSector r) (xyrToSector xyr)
  wierdLookingMap       = gradient >>> (fromMap (transform (mandelmap n) xform) $ resToSector (Resolution 1920 1080))
