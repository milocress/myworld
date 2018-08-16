{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Transform where

import PlanarCoordinate

newtype Transform = Transform { runTransform :: PlanarCoordinate -> PlanarCoordinate }

class Transformable a where
  transform :: a -> Transform -> a

instance Transformable Sector where
  transform (Sector tl br) t = Sector (runTransform t tl) (runTransform t br)

instance Transformable PlanarCoordinate where
  transform p t = runTransform t p

scale :: Scalar -> Transform
scale a = Transform $ \(x, y) -> (x * a, y * a)

translate :: PlanarCoordinate -> Transform
translate (a, b) = Transform $ \(x, y) -> (x + a, y + b)

mapCoordinates :: Sector -> Sector -> Transform
mapCoordinates a@(Sector tl _) b@(Sector tl' _) =
  let c = width b / width a
      (Transform f) = translate (-tl)
      (Transform g) = scale c
      (Transform h) = translate tl'
  in Transform $ h .g . f
