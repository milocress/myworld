module Resolution where

import PlanarCoordinate 

data Resolution = Resolution { image_width  :: Int
                             , image_height :: Int} deriving (Show)

resToSector :: Resolution -> Sector
resToSector (Resolution x y) = Sector
                               (0, fromIntegral y)
                               (fromIntegral x, 0)
