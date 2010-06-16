{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TensorAlgebra(module Basis, TensorAlgebra, TensorAlgebraBasis,
  freeTA, diag) where
import Algebra
import qualified Restricted as R
import Data.List(intercalate)
import qualified Data.Set as S
import Grading
import Basis
import Steenrod
import Z2

newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

-- Some modules need to examine elementary tensors, but I don't want to pollute
-- the namespace.
type TensorAlgebraBasis b = Basis b

instance Show b => Show (Basis b) where
  show (Basis []) = "1"
  show (Basis xs) = intercalate " \\otimes " . map show $ xs

instance Num r => Multiplicative r (Basis b) where
  one = [(pack [], 1)]
  mul xs ys = [(lift2 (++) xs ys, 1)]

-- For TensorAlgebra r b to have a computable grading, b must be
-- positively-graded and finitary
instance (Ord b, Graded b) => Graded (Basis b) where
  degree = sum . map degree . unpack
  grading = S.fromList . map pack . g where
    g n = case compare n 0 of
      LT -> []
      EQ -> [[]]
      GT -> do
        m <- [1..n]
        b <- S.toList $ grading m
        bs <- g (n - m)
        return $ b:bs

type TensorAlgebra r b = FreeModule r (Basis b)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = inject . fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f . unpack

-- TensorAlgebra r b is a free r-algebra
freeTA :: (Module r m, Num m) => (b -> m) -> TensorAlgebra r b -> m
freeTA f = freeM (product . map f . unpack)

-- The diagonal action on the tensor algebra and its quotients
diag :: (Ord (f b), Show (f b), Multiplicative Z2 (f b), R.MonadR f b,
  AModule b) => Integer -> [b] -> FreeModule Z2 (f b)
diag n [] | n == 0    = 1
          | otherwise = 0 -- Sq^n 1 = 0 if n /= 0
diag n (x:xs) = sum [ include (sq' t x) * diag (n-t) xs | t <- [0..n] ]

instance (Ord b, Show b, AModule b) => AModule (Basis b) where
  sq' n = diag n . unpack
