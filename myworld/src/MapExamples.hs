{-# LANGUAGE LambdaCase #-}
module MapExamples where

import Coordinate
import Map

type MaybeHeightmap = MapT Maybe Height

flatMap :: MaybeHeightmap
flatMap = return 1.0

-- The unit declaration above is synonymous with the following, but more readable (and therefore preferred):
-- flatMap = Map $ \_ -> Just 1.0

-- Any calls to getPointAttr of flatMap will return 1.0 -- this is the most basic and simple kind of map.
-- Ex:
-- getPointAttr flatMap (Planar 1 2)
-- getPointAttr flatMap (Spherical Infinity 0.0)
-- As we can see, unit declarations don't sanity-check inputs, but don't need to. They can take any input.

bumpyMap :: MaybeHeightmap
bumpyMap = MapT $ \case
  Planar x y -> Just $ sin x * cos y
  _          -> Nothing

-- Note that since bumpyMap works around the Maybe monad, it's possible for the function to fail, such as when
-- it's applied to a coordinate that is non-planar.
-- Richer monadic wrappers can result in more expressive error messages, such as (Left "spherical coordinates not
-- supported" -- a result of the Either monad).
type MaybeHeightmap' = MapT (Either String) Height

bumpyMap' :: MaybeHeightmap'
bumpyMap' = MapT $ \case
  Planar x y -> Right $ sin x * cos y
  _          -> Left "Spherical coordinates not supported"

-- Now, we can use monadic binding to compose maps! Yay!
flatAndBumpy :: MaybeHeightmap
flatAndBumpy = do
  h  <- flatMap
  h' <- bumpyMap
  return $ h + h'
-- This is remarkably compact syntax compared to a non-monadic style, and forms the basis for the implementation
-- of the Num typeclass (the Num implementation has since been rewritten to use the more succinct Applicative
-- syntax, but it is easy to imagine how Num could be written with monads).

-- instance (Monad m, Num a) => Num (MapT m a) where
--   a + b = do
--     v  <- a
--     v' <- b
--     return $ v + v'
--   ...
--   abs a = do
--     v <- a
--     return $ abs v
--   fromInteger = return . fromInteger

-- As a matter of fact, now that arithmetic is implemented for Maps, it's much simpler to define flatAndBumpy:
flatAndBumpy' :: MaybeHeightmap
flatAndBumpy' = flatMap + bumpyMap

-- These expressions become even more powerful when used to transform types. For example, imagine that a
-- topographical map (hMap :: heightmap), a roughness map (rMap :: MapT Maybe Double), and a wetmap (wMap :: MapT
-- Maybe Bool) are to be displayed by a program as an image, using the red, green, and blue channels to indicate
-- elevation, roughness, and water distribution, respectively. A simple color library exists which has the type
-- constructor:
-- RGB :: Double -> Double -> Double -> Color
data Color = RGB { red :: Double, blue :: Double, green :: Double }
-- In order to display the map, we need to convert the heightmap to a Color map.
type Colormap = Map Color
-- We can use monads for this!
toColorMap :: Heightmap -> Map Double -> Map Bool -> Colormap
toColorMap hMap rMap wMap = do
  height    <- hMap
  roughness <- rMap
  wetness   <- wMap
  let w_val = if wetness then 1 else 0
    in return $ RGB height roughness w_val

-- We can also use applicatives.
toColorMap' :: Heightmap -> Map Double -> Map Bool -> Colormap
toColorMap' hMap rMap wMap = RGB <$> hMap <*> rMap <*> fmap (\x -> if x then 1 else 0) wMap
-- While the monadic version is the more readable of the two (but not by a large margin), the applicative version
-- is far more succinct. At this point it's debatable which style to use. I believe that for calculations which
-- require a great deal of nested unwrapping, composition, and/or mapping, a monadic style is appropriate.
-- For simpler calculations (such as the one above) which are more a matter of applying a function to the value
-- contained by a monad rather than to the monad itself (with minimal nesting such as the if-else block), the
-- applicative style is cleaner.

-- On the other hand, complex calculations, which "require a great deal of nested unwrapping, composition, and/or"
-- mapping, can be composed of simpler applicative-style functions. I'm honestly not sure what the monadic style is
-- useful for at this point, but it's implementation will be preserved in case I think of something.


runMaybeMap :: MapT Maybe a -> a -> Coordinate -> a
runMaybeMap m failval = runMap . MapT $ \p ->
  case runMapT m p of
    Just x  -> return x
    Nothing -> return failval

-- Another nice thing is that we can implement custom logic to change the underlying monad of a MapT (or map for
-- that matter)
