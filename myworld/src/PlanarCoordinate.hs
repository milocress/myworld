{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module PlanarCoordinate where

type Scalar = Double

type PlanarCoordinate = (Scalar, Scalar)
x_coord, y_coord :: PlanarCoordinate -> Scalar
x_coord = fst
y_coord = snd

instance Num PlanarCoordinate where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (a, b)      = (abs a, abs b)
  signum (a, b)   = (signum a, signum b)
  fromInteger a   = (fromInteger a, 0)
  negate (a, b)   = (-a, -b)

data Sector = Sector { top_left     :: PlanarCoordinate
                     , bottom_right :: PlanarCoordinate } deriving (Show)

inSector :: Sector -> PlanarCoordinate -> Bool
inSector (Sector (a, b) (c, d)) (x, y) =
  between_inclusive a c x &&
  between_inclusive d b y

between_inclusive :: Scalar -> Scalar -> Scalar -> Bool
between_inclusive bot top x = x >= bot && x <= top

width :: Sector -> Scalar
width (Sector (a, _) (b, _)) = b - a

height :: Sector -> Scalar
height (Sector (a, _) (b, _)) = b - a
