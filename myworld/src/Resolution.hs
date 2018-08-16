module Resolution where

import PlanarCoordinate 

data Resolution = Resolution { image_width  :: Int
                             , image_height :: Int} deriving (Show)

resToSector :: Resolution -> Sector
resToSector (Resolution x y) = Sector
                               (0, fromIntegral y)
                               (fromIntegral x, 0)

data XYR = XYR { xyr_x :: Double, xyr_y :: Double, xyr_r :: Double }
xyrToSector :: XYR -> Sector
xyrToSector (XYR x y r) = Sector (x - r, y + r) (x + r, y - r)
