module SectorMap where

import Data.Functor.Identity
import Control.Applicative

import Map
import PlanarCoordinate

type SectorMap a = MapT Maybe a

runSectorMap :: SectorMap a -> PlanarCoordinate -> Maybe a
runSectorMap = runMapT


instance Alternative Identity where
  (<|>) = const
  empty = undefined

-- Non-strict Map addition
(<+>) :: (Alternative m, Num a) => MapT m a -> SectorMap a -> MapT m a
bot <+> top = MapT $ \p ->
  case runSectorMap top p of
    Nothing -> runMapT bot p
    Just x  -> (+x) <$> runMapT bot p <|> pure x

-- Non-strict Map overlaying
(>>>) :: (Applicative m) => MapT m a -> SectorMap a -> MapT m a
bot >>> top = MapT $ \p ->
  case runSectorMap top p of
    Nothing -> runMapT bot p
    Just x  -> pure x

-- Note that the below code will not work, because monadic binding
-- (>>=) doesn't allow a monad transformer to change the underlying
-- monad.
-- (>>>) :: (Applicative m) => MapT m a -> SectorMap a -> MapT m a
-- bot >>> top = do
--   v <- top
--   case v of
--     Nothing -> bot
--     Just x -> pure x

fromMap :: Map a -> Sector -> SectorMap a
fromMap m s = MapT $ \p ->
  if inSector s p
  then Just $ runMap m p
  else Nothing

toMap :: SectorMap a -> a -> Map a
toMap m failval = return failval >>> m

emptySectorMap :: SectorMap a
emptySectorMap = MapT $ \_ -> Nothing
