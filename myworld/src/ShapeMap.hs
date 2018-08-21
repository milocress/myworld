module ShapeMap where

import Map
import Transform
import PlanarCoordinate

type ShapeMap = Map Double

buildShapeMap :: Sector -> PlanarCoordinate -> Int -> ShapeMap
buildShapeMap s p n = transform (MapT $ \q ->
  let x = (-1) * logBase 2 (distance p q)
  in return $ min (fromIntegral n) x) (mapCoordinates s $ Sector (-1, 1) (1, -1))
