module BoundedMap where

import Map
import PlanarCoordinate

type BoundedMap a = MapT Maybe a

runBoundedMap :: BoundedMap a -> PlanarCoordinate -> Maybe a
runBoundedMap = runMapT

-- Non-strict Map addition
(<+>) :: (Functor m, Num a) => MapT m a -> BoundedMap a -> MapT m a
bot <+> top = MapT $ \p ->
  case runBoundedMap top p of
    Nothing -> runMapT bot p
    Just x  -> (+x) <$> runMapT bot p

-- Non-strict Map overlaying
(>>>) :: (Applicative m) => MapT m a -> BoundedMap a -> MapT m a
bot >>> top = MapT $ \p ->
  case runBoundedMap top p of
    Nothing -> runMapT bot p
    Just x  -> pure x

fromMap :: Map a -> Sector -> BoundedMap a
fromMap m s = MapT $ \p ->
  if inSector s p
  then Just $ runMap m p
  else Nothing

toMap :: BoundedMap a -> a -> Map a
toMap m failval = return failval >>> m
