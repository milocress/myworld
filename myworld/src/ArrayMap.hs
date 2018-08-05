module ArrayMap where

import Data.Array.Repa
import qualified Data.Vector.Unboxed as U

import Map
import BoundedMap
import PlanarCoordinate
import Resolution

type ArrayMap a = BoundedMap a

runArrayMap :: ArrayMap a -> PlanarCoordinate -> Maybe a
runArrayMap = runBoundedMap

fromArray :: U.Unbox a => Array U DIM2 a -> ArrayMap a
fromArray arr =
  let (Z :. a :. b) = extent arr
  in fromMap (MapT $ \(Planar x y) -> return $ arr ! (Z :. floor x :. floor y)) (resToSector $ Resolution a b)
