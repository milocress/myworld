module Coordinate where

data Coordinate = Planar { x_coord :: Double
                         , y_coord :: Double }
                | Spherical { latitude  :: Double
                            , longitude :: Double }
                | Cubic { x_coord :: Double,
                          y_coord :: Double,
                          z_coord :: Double
                        }

newtype Transform = Transform { runTransform :: Coordinate -> Coordinate }
