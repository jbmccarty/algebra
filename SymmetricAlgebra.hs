{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
-- dirty hack for KZ2; it should use a newtype instead

module SymmetricAlgebra(module Basis, Basis(), FreeSymmetricAlgebra,
  SymmetricAlgebra)
where
import FreeModuleBase
import Algebra
import qualified Restricted as R
import qualified Data.Map as M
import qualified Data.Set as S
import Grading
import Basis
import Steenrod
import Data.List(genericReplicate, intersperse)
import TensorAlgebra(diag)
import Control.Arrow((***))

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

instance ViewBasis (Basis b) [b] where
  viewBasis = concatMap (uncurry $ flip genericReplicate) . M.toList . unpack

showsPrecB :: Show b => Int -> FM Integer b -> ShowS
showsPrecB p = s p . M.toList where
  s p [] = showString "1"
  s p [b] = showsFactor p b
  s p xs = showParen (p > prec) . foldr (.) id . intersperse (showString " * ")
           . map (showsFactor prec) $ xs
  showsFactor p (x, 1) = showsPrec p x
  showsFactor p (x, n) = showParen (p > prec+1) $ showsPrec (prec+1) x
                         . showString "^{" . showsPrec 0 n . showString "}"
  prec = 7

instance Show b => Show (Basis b) where
  showsPrec p = showsPrecB p . unpack

instance Ord b' => R.Functor Basis b b' where
  fmap = lift . fmapFM

instance R.MonadR Basis b where
  return = pack . returnFM

instance (Num r, Ord b) => Multiplicative r (Basis b) where
  one = [(pack zeroFM, 1)]
  mul x y = [(lift2 addFM x y, 1)]

instance (Ord b, Graded b) => Graded (Basis b) where
  degree = sum . map (\(b, n) -> n*degree b) . M.toList . unpack
  -- this is woefully inefficient, but a correct implementation is tricky
  grading = S.fromList . map (pack . M.fromListWith (+)) . g where
    g n = case compare n 0 of
      LT -> []
      EQ -> [[]]
      GT -> do
        m <- [1..n]
        b <- S.toList $ grading m
        bs <- g (n - m)
        return $ (b, 1):bs

instance (Ord b, Bigraded b) => Bigraded (Basis b) where
  bidegree = both sum . unzip . map f . M.toList . unpack
    where f (b, n) = both (n*) $ bidegree b
          both g = g *** g
  -- also woefully inefficient
  bigrading = S.fromList . map (pack . M.fromListWith (+)) . g where
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
        return $ (b, 1):bs

type FreeSymmetricAlgebra r b = FreeModule r (Basis b)
type SymmetricAlgebra m = FreeSymmetricAlgebra (URing m) (UBasis m)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = inject . R.fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f' . M.toList . unpack where
    f' (x, n) = (f x)^n

-- SymmetricAlgebra r b is a free commutative r-algebra

-- The symmetric algebra is the quotient of the tensor algebra by the ideal
-- generated by elements of the form a⊗b - b⊗a. Sq^k on an element of this form
-- is a sum of such elements, so the diagonal action on the tensor algebra
-- descends to the symmetric algebra.

-- I'm sure there's a more efficient algorithm than using diag directly.
instance (Ord b, Show b, AModule b) => AModule (Basis b) where
  sq' n = diag n . viewBasis
