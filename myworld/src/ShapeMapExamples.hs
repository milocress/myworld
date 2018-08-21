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
    f          = sampleCenter $ mandelmap 300
    sec        = xyrToSector xyr
    shapeMap   = return 12 -- (return 2) * (buildShapeMap sec focusPoint n)
    focusPoint = (0.29, 0.015)

lowPolyMandelImg :: Int -> XYR -> Resolution -> DynamicImage
lowPolyMandelImg n xyr r = fromRGBMap lowPolyMandelMap' r where
  xform             = mapCoordinates (resToSector r) (xyrToSector xyr)
  lowPolyMandelMap' = transform (lowPolyMandelMap n xyr) xform
