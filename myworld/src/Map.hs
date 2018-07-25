module Map where
import Coordinate
import Data.Functor.Identity
import Control.Monad.Trans.Class

newtype MapT m a = MapT { runMapT :: Coordinate -> m a }

instance Functor m => Functor (MapT m) where
  fmap f x = MapT $ \p -> fmap f ( runMapT x p )

instance Applicative m => Applicative (MapT m) where
  f <*> x = MapT $ \p -> runMapT f p <*> runMapT x p
  pure  x = MapT $ \_ -> pure x

instance Monad m => Monad (MapT m) where
  (>>=)  = bind
  return = unit

bind :: Monad m => MapT m a -> (a -> MapT m b) -> MapT m b
bind x f = MapT $ \p -> do
  pointAttr <- runMapT x p
  runMapT ( f pointAttr ) p

unit :: Monad m => a -> MapT m a
unit x = MapT $ \_ -> return x

instance (Applicative m, Num a) => Num (MapT m a) where
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b
  abs a = abs <$> a
  signum a = signum <$> a
  fromInteger = pure . fromInteger

instance MonadTrans MapT where
  lift = MapT . const

type Height = Double
type Heightmap = Map Height
type Map = MapT Identity

runMap :: Map a -> Coordinate -> a
runMap m p = runIdentity (runMapT m p)
