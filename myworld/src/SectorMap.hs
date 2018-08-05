module SectorMap where

import Map
import PlanarCoordinate

type SectorMap a = MapT Maybe a

runSectorMap :: SectorMap a -> PlanarCoordinate -> Maybe a
runSectorMap = runMapT

-- Non-strict Map addition
(<+>) :: (Functor m, Num a) => MapT m a -> SectorMap a -> MapT m a
bot <+> top = MapT $ \p ->
  case runSectorMap top p of
    Nothing -> runMapT bot p
    Just x  -> (+x) <$> runMapT bot p

-- Non-strict Map overlaying
(>>>) :: (Applicative m) => MapT m a -> SectorMap a -> MapT m a
bot >>> top = MapT $ \p ->
  case runSectorMap top p of
    Nothing -> runMapT bot p
    Just x  -> pure x

fromMap :: Map a -> Sector -> SectorMap a
fromMap m s = MapT $ \p ->
  if inSector s p
  then Just $ runMap m p
  else Nothing

toMap :: SectorMap a -> a -> Map a
toMap m failval = return failval >>> m
