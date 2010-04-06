{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SymmetricAlgebra(module Basis, SymmetricAlgebra) where
import FreeModuleBase
import Algebra
import qualified Restricted as R
import qualified Data.Map as M
import Basis

-- The symmetric algebra could be implemented as a quotient of the tensor
-- algebra, but I would prefer to have a canonical basis.

-- should include only positive powers
-- This is isomorphic to FreeModule Integer b, but I need access to the
-- internals
newtype Basis b = Basis { unBasis :: FM Integer b } deriving (Eq, Ord)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

showB :: Show b => FM Integer b -> String
showB = s . M.toList where
  s [] = "1"
  s [x] = showTerm x
  s (x:xs) = showTerm x ++ " " ++ s xs
  showTerm (x, 1) = show x
  showTerm (x, n) = show x ++ "^{" ++ show n ++ "}"

instance Show b => Show (Basis b) where
  show = showB . unpack

instance Ord b' => R.Functor Basis b b' where
  fmap = lift . fmapFM

instance R.MonadR Basis b where
  return = pack . returnFM

instance (Num r, Ord b) => Multiplicative r (Basis b) where
  one = [(pack zeroFM, 1)]
  mul x y = [(lift2 addFM x y, 1)]

type SymmetricAlgebra r b = FreeModule r (Basis b)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = inject . R.fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f' . M.toList . unpack where
    f' (x, n) = (f x)^n

-- SymmetricAlgebra r b is a free commutative r-algebra
