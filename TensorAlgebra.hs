{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TensorAlgebra(module Basis, FreeTensorAlgebra, TensorAlgebra,
TensorAlgebraBasis, freeTA, diag', diag) where
import Algebra
import qualified Restricted as R
import Data.List(intersperse)
import qualified Data.Set as S
import Grading
import Basis
import Steenrod
import Z2
import Control.Arrow((***))

newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

-- Some modules need to examine elementary tensors, but I don't want to pollute
-- the namespace.
type TensorAlgebraBasis b = Basis b

instance ViewBasis (Basis b) [b] where
  viewBasis = unpack

instance Show b => Show (Basis b) where
  showsPrec p (Basis []) = showString "1"
  showsPrec p (Basis [b]) = showsPrec p b
  showsPrec p (Basis bs) = showParen (p > prec) . foldr (.) id .
                           intersperse (showString " \\otimes ") .
                           map (showsPrec prec) $ bs
    where prec = 6

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

-- b should be positively-graded and finitary
instance (Ord b, Bigraded b) => Bigraded (Basis b) where
  bidegree = (sum *** sum) . unzip . map bidegree . unpack
  bigrading = S.fromList . map pack . g where
    g (i, j) = case (compare i 0, compare j 0) of
      (LT, _) -> []
      (_, LT) -> []
      (EQ, GT) -> []
      (GT, EQ) -> []
      (EQ, EQ) -> [[]]
      (GT, GT) -> do
        i' <- [1..i]
        j' <- [1..j]
        b <- S.toList $ bigrading (i', j')
        bs <- g (i-i', j-j')
        return $ b:bs

type FreeTensorAlgebra r b = FreeModule r (Basis b)
type TensorAlgebra m = FreeTensorAlgebra (URing m) (UBasis m)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = inject . fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f . unpack

-- TensorAlgebra r b is a free r-algebra
freeTA :: (Module r m, Num m) => (b -> m) -> FreeTensorAlgebra r b -> m
freeTA f = freeM (product . map f . unpack)

-- More general version of diag, useful for DyerLashof
diag' :: (Ord (f b), Show (f b), Multiplicative Z2 (f b)) =>
  (Integer -> b -> FreeModule Z2 (f b)) -> Integer -> [b] -> FreeModule Z2 (f b)
diag' s n [] | n == 0 = 1
             | otherwise = 0 -- Sq^n 1 = 0 if n /= 0
diag' s n (x:xs) = sum [ s t x * diag' s (n-t) xs | t <- [0..n] ]

-- The diagonal action on the tensor algebra and its quotients
diag :: (Ord (f b), Show (f b), Multiplicative Z2 (f b), R.MonadR f b,
  AModule b) => Integer -> [b] -> FreeModule Z2 (f b)
diag = diag' (\n -> include . sq' n)

instance (Ord b, Show b, AModule b) => AModule (Basis b) where
  sq' n = diag n . unpack
