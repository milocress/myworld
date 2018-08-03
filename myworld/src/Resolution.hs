module Resolution where

import PlanarCoordinate 

data Resolution = Resolution { image_width  :: Int
                             , image_height :: Int} deriving (Show)

resToSector :: Resolution -> Sector
resToSector (Resolution x y) = Sector
                               (Planar 0 (fromIntegral y))
                               (Planar (fromIntegral x) 0)
