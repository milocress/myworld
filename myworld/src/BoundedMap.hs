module BoundedMap where

import Map
import PlanarCoordinate


type BoundedMap a = MapT Maybe a

layerAdd :: (Num a) => Map a -> BoundedMap a -> Map a
bot `layerAdd` top = MapT $ \p ->
  let a = runIdentity $ runMapT bot p
      b = runMapT top p
  in case b of
    Just x  -> return $ a + x
    Nothing -> return   a
