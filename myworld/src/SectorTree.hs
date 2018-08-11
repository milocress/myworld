{-# LANGUAGE DeriveFunctor #-}
module SectorTree where

import Data.Functor.Foldable

import SectorMap

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
  alg (SectorNodeF f fs) = foldr (\x acc -> x . acc) f fs
