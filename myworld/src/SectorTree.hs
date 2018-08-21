{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
module SectorTree where

import Data.Functor.Foldable

import Map
import SectorMap
import PlanarCoordinate
import ShapeMap

-- type SectorFunc a = (forall m . MapT m a -> SectorTree a)
type SectorFunc a = SectorMap a -> SectorMap a

data SectorTreeF a r = SectorNodeF (SectorFunc a) [r]
                     deriving (Functor)

type SectorTree a = Fix (SectorTreeF a)

{-
bigMap :: SectorMap a
littleMap :: SectorMap a
bigMap >>> littleMap :: SectorMap a
(>>> littleMap) :: SectorMap a -> SectorMap a
f = (>>> littleMap)
g = (<+> teensyMap)
f . g     = (<+> teensyMap) (>>> littleMap)
(f . g) x = (x <+> teensyMap) >>> littleMap
-}

compileSectorTree :: SectorTree a -> SectorMap a
compileSectorTree t = (cata alg t) emptySectorMap where
  alg (SectorNodeF f fs) = foldr (.) f fs

type SectorSeed = (Sector, ShapeMap, Int) -- The Double is Recursion Depth

buildSectorTree :: (SectorSeed -> SectorFunc a) -> SectorSeed -> SectorTree a
buildSectorTree f t = ana coalg t where
  coalg seed@(s@(Sector tl br), m, d) =
    let
    children =
      if (runMap m (midpoint tl br) > 0)
      then (map (\s' -> (s', (subtract 1) <$> m, d + 1)) $ subdivideSector s)
      else []
    in SectorNodeF (f seed) children
