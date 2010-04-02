{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- free modules over a ring, including multiplicative structures
module FreeModule(module Module, FreeModule(), returnList) where
import qualified Restricted as R
import FreeModuleBase
import Module
import Data.List(foldl')

--------------------------------------------------------------------------------
-- The user-visible type and associated functions

newtype FreeModule r b = CFM { unCFM :: FM r b } deriving (Eq, Ord)

pack = CFM
unpack = unCFM
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

-- A free module is a (restricted) functor:
instance (Num r, Ord b') => R.Functor (FreeModule r) b b' where
  fmap = lift . fmapFM

-- A free module is a (restricted) monad:
instance Num r => R.MonadR (FreeModule r) b where
  return = pack . returnFM

instance (Num r, Ord b') => R.Monad (FreeModule r) b b' where
  x >>= f = pack $ bindFM (unpack x) (unpack . f)

-- returnList takes a list of (basis element, coefficient) pairs and returns
-- their sum in the free module
returnList :: (Num r, Ord b) => [(b, r)] -> FreeModule r b
returnList = foldl' (.+.) zero . map (\(b, r) -> r .* R.return b)

instance (Num r, Show b) => Show (FreeModule r b) where
  show = showFM . unpack

-- A free module is an abelian group
instance (Num r, Ord b) => AbelianGroup (FreeModule r b) where
  (.+.) = lift2 addFM
  zero = pack zeroFM
  negateA = lift negateFM

-- A free module is a module over its ring
instance (Num r, Ord b) => Module r (FreeModule r b) where
  r .* x = lift (actFM r) x

-- A free module is free
instance (Num r, Ord b) => Free r (FreeModule r b) b where
  inject = pack . injectFM
  free f = freeFM f . unpack
