module ShapeMap where

import PlanarCoordinate
import Map

type ShapeMap = Map Double

buildShapeMap :: PlanarCoordinate -> ShapeMap
buildShapeMap p = MapT $ \q -> return $ logBase 2 (distance p q)
