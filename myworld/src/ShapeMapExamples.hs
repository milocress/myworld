module ShapeMapExamples where

import Codec.Picture

import Map
import RGBMap
import ShapeMap
import Transform
import SectorMap
import Resolution
import SectorTree
import RepaExamples
import PlanarCoordinate
-- The imports are in order of length, just for fun.

sampleCenter :: Map a -> SectorSeed -> SectorFunc a
sampleCenter m (s@(Sector tl br), _, _) = \x -> x >>> (fromMap (return $ runMap m $ midpoint tl br) s)

invertColors :: SectorSeed -> SectorFunc RGB8
invertColors (s, _, d) = \x -> x >>> (fromMap (return $ if even d then black else white) s)

lowPolyMandelMap :: Int -> XYR -> RGBMap
lowPolyMandelMap n xyr@(XYR x y _) =
  (return black) >>> (compileSectorTree $ buildSectorTree f (sec, shapeMap, 0)) where
    f          = invertColors -- sampleCenter $ mandelmap 300
    sec        = xyrToSector xyr
    shapeMap   = return 1 + (buildShapeMap focusPoint n)
    focusPoint = (x, y)

lowPolyMandelImg :: Int -> XYR -> Resolution -> DynamicImage
lowPolyMandelImg n xyr r = fromRGBMap lowPolyMandelMap' r where
  xform             = mapCoordinates (resToSector r) (xyrToSector xyr)
  lowPolyMandelMap' = transform (lowPolyMandelMap n xyr) xform

shapeToRGB8 :: Int -> Double -> RGB8
shapeToRGB8 max x = (x', x', x') where x' = if x <= 0 then 0 else floor $ 255 * (x / (fromIntegral max))

shapeMapImg :: Int -> XYR -> Resolution -> DynamicImage
shapeMapImg n xyr@(XYR x y _) res = fromRGBMap shapeMap' res where
  xform     = mapCoordinates (resToSector res) (xyrToSector xyr)
  shapeMap' = transform (shapeToRGB8 n <$> (buildShapeMap (x, y) n)) xform
