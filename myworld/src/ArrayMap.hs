module ArrayMap where

import Data.Array.Repa
import qualified Data.Vector.Unboxed as U

import Map
import SectorMap
import PlanarCoordinate
import Resolution

type ArrayMap a = SectorMap a

runArrayMap :: ArrayMap a -> PlanarCoordinate -> Maybe a
runArrayMap = runSectorMap

fromArray :: U.Unbox a => Array U DIM2 a -> ArrayMap a
fromArray arr =
  let (Z :. a :. b) = extent arr
  in fromMap (MapT $ \(x, y) -> return $ arr ! (Z :. floor x :. floor y)) (resToSector $ Resolution a b)
