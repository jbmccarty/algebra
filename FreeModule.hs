{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- free modules over a ring, including multiplicative structures
module FreeModule(module Module, FreeModule(), UBasis, URing, inject, freeM,
  returnList) where
import qualified Restricted as R
import FreeModuleBase
import Module
import Data.List(foldl')

--------------------------------------------------------------------------------
-- The user-visible type and associated functions

newtype FreeModule r b = CFM { unCFM :: FM r b } deriving (Eq, Ord)

-- The underlying basis of a free module
type family UBasis a :: *
type instance UBasis (FreeModule r b) = b

-- The associated ring of a free module
type family URing a :: *
type instance URing (FreeModule r b) = r

pack = CFM
unpack = unCFM
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

instance (Num r, Show b) => Show (FreeModule r b) where
  showsPrec d = showsPrecFM d . unpack

-- FreeModule r b is an abelian group
instance (Num r, Ord b) => AbelianGroup (FreeModule r b) where
  (.+.) = lift2 addFM
  zero = pack zeroFM
  negateA = lift negateFM

-- FreeModule r b is an r-module
instance (Num r, Ord b) => Module r (FreeModule r b) where
  r .* x = lift (actFM r) x

-- FreeModule r b is a free r-module: i.e., freeFM f is the unique r-module map
-- such that freeM f . inject = f.
inject :: Num r => b -> FreeModule r b
inject = pack . injectFM

freeM :: Module r m => (b -> m) -> FreeModule r b -> m
freeM f = freeFM f . unpack

-- A free module is a (restricted) functor:
instance (Num r, Ord b') => R.Functor (FreeModule r) b b' where
  fmap = lift . fmapFM -- fmap f = freeM (inject . f)

-- A free module is a (restricted) monad:
instance Num r => R.MonadR (FreeModule r) b where
  return = pack . returnFM -- = inject

instance (Num r, Ord b') => R.Monad (FreeModule r) b b' where
  x >>= f = pack $ bindFM (unpack x) (unpack . f) -- = freeM f x

-- returnList takes a list of (basis element, coefficient) pairs and returns
-- their sum in the free module
returnList :: (Num r, Ord b) => [(b, r)] -> FreeModule r b
returnList = foldl' (.+.) zero . map (\(b, r) -> r .* R.return b)
