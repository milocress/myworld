module PlanarCoordinate where

type Scalar = Double

data PlanarCoordinate = Planar { x_coord :: Scalar
                               , y_coord :: Scalar } deriving (Show)

instance Num PlanarCoordinate where
  (Planar a b) + (Planar c d) = Planar (a + c) (b + d)
  (Planar a b) * (Planar c d) = Planar (a * c) (b * d)
  abs (Planar a b) = Planar (abs a) (abs b)
  signum (Planar a b) = Planar (signum a) (signum b)
  fromInteger a = Planar (fromInteger a) 0
  negate (Planar a b) = Planar (-a) (-b)

newtype Transform = Transform { runTransform :: PlanarCoordinate -> PlanarCoordinate }

data Sector = Sector { top_left     :: PlanarCoordinate
                     , bottom_right :: PlanarCoordinate } deriving (Show)

width :: Sector -> Scalar
width (Sector (Planar a _) (Planar b _)) = b - a

height :: Sector -> Scalar
height (Sector (Planar a _) (Planar b _)) = b - a

class Transformable a where
  transform :: a -> Transform -> a

instance Transformable Sector where
  transform (Sector tl br) t = Sector (runTransform t tl) (runTransform t br)

scale :: Scalar -> Transform
scale a = Transform $ \(Planar x y) -> Planar (x * a) (y * a)

translate :: PlanarCoordinate -> Transform
translate (Planar a b) = Transform $ \(Planar x y) -> Planar (x + a) (y + b)

mapCoordinates :: Sector -> Sector -> Transform
mapCoordinates a@(Sector tl _) b@(Sector tl' _) =
  let c = width b / width a
      (Transform f) = translate (-tl)
      (Transform g) = scale c
      (Transform h) = translate tl'
  in Transform $ h .g . f
