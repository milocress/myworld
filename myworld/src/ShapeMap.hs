module ShapeMap where

import Map
import Transform
import PlanarCoordinate

type ShapeMap = Map Double

buildShapeMap :: PlanarCoordinate -> Int -> ShapeMap
buildShapeMap p n = MapT $ \q ->
  let x = (-1) * logBase 2 (distance p q)
  in return $ min (fromIntegral n) x
