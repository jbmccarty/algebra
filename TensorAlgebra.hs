{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TensorAlgebra(module Algebra, TensorAlgebra, TensorAlgebraBasis,
  packTAB, unpackTAB, freeTA) where
import Algebra
import qualified Restricted as R
import Data.List(intercalate)
import Grading
import Basis

newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

-- Some modules need to examine elementary tensors, but I don't want to pollute
-- the namespace.
type TensorAlgebraBasis b = Basis b
packTAB = pack
unpackTAB = unpack

instance Show b => Show (Basis b) where
  show (Basis []) = "1"
  show (Basis xs) = intercalate " \\otimes " . map show $ xs

instance Num r => Multiplicative r (Basis b) where
  one = [(pack [], 1)]
  mul xs ys = [(lift2 (++) xs ys, 1)]

instance Graded b => Graded (Basis b) where
  degree = sum . map degree . unpack
  grading = undefined -- too lazy to implement just now

type TensorAlgebra r b = FreeModule r (Basis b)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = inject . fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f . unpack

-- TensorAlgebra r b is a free r-algebra
freeTA :: (Module r m, Num m) => (b -> m) -> TensorAlgebra r b -> m
freeTA f = freeM (product . map f . unpack)
