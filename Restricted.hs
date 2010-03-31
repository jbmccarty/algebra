{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- restricted monads, adapted from
-- http://okmij.org/ftp/Haskell/types.html#restricted-datatypes
module Restricted(Functor(..), MonadR(..), Monad(..)) where
import qualified Prelude as P

class Functor f a b where
  fmap :: (a -> b) -> (f a -> f b)

instance P.Functor f => Functor f a b where
  fmap = P.fmap

class MonadR m a where
  return :: a -> m a

class (MonadR m a, MonadR m b) => Monad m a b where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= P.const y

instance P.Monad m => MonadR m a where
  return = P.return

instance P.Monad m => Monad m a b where
  (>>=) = (P.>>=)
