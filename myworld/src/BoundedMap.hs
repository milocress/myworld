module BoundedMap where

import Map
import PlanarCoordinate


type BoundedMap a = MapT Maybe a

runBoundedMap :: BoundedMap a -> PlanarCoordinate -> Maybe a
runBoundedMap = runMapT

-- Layer overlapping
(<+>) :: (Functor m, Num a) => MapT m a -> BoundedMap a -> MapT m a
bot <+> top = MapT $ \p ->
  case runBoundedMap top p of
    Nothing -> runMapT bot p
    Just x  -> (+x) <$> runMapT bot p


(>>>) :: (Applicative m) => MapT m a -> BoundedMap a -> MapT m a
bot >>> top = MapT $ \p ->
  case runBoundedMap top p of
    Nothing -> runMapT bot p
    Just x  -> pure x
