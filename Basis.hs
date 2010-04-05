{-# LANGUAGE MultiParamTypeClasses #-}

module Basis where
import FreeModule
import qualified Restricted as R

-- If f is a type constructor, the composition FreeModule r . f may be a
-- functor even if f is not. Such a type constructor just needs to describe
-- what happens on basis elements in (f b).
class BasisFunctor f b b' where
  fmapB' :: Num r => (b -> b') -> f b -> FreeModule r (f b')

-- it's debatable whether this instance is a good idea
{-
instance R.Functor f b b' => BasisFunctor f b b' where
  fmapB' f = inject . R.fmap f
-}

-- haskell doesn't allow partially-applied type synonyms, else this could be an
-- instance of R.Functor.
fmapB :: (Num r, Ord (f b'), BasisFunctor f b b') =>
  (b -> b') -> FreeModule r (f b) -> FreeModule r (f b')
fmapB f x = x R.>>= fmapB' f

-- The same thing for monads...there is probably a better way to implement
-- this.
class R.MonadR f b' => BasisMonad f b b' where
  bindB' :: Num r => (b -> FreeModule r (f b')) -> f b -> FreeModule r (f b')

injectB :: (Num r, R.MonadR f b) => b -> FreeModule r (f b)
injectB = inject . R.return

bindB :: (Num r, Ord (f b'), BasisMonad f b b') => FreeModule r (f b)
  -> (b -> FreeModule r (f b')) -> FreeModule r (f b')
bindB x f = x R.>>= bindB' f

-- If f is injective, then FreeModule r b includes into FreeModule r (f b).
include :: (Num r, Ord (f b), R.MonadR f b)
  => FreeModule r b -> FreeModule r (f b)
include = R.fmap R.return
