{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExteriorAlgebra(module Basis, FreeExteriorAlgebra, ExteriorAlgebra) where
import Algebra
import Z2
import qualified Restricted as R
import qualified Data.Map as M
import Basis
import Steenrod
import TensorAlgebra(diag)
import Data.List(intersperse, insert)
import Grading
import qualified Data.Set as S
import Control.Arrow((***))
import Control.Monad(guard)

-- The exterior algebra could be implemented as a quotient of the tensor
-- algebra, but I would prefer to have a canonical basis.

-- should always be sorted and include no duplicates
newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

showsPrecB :: Show b => Int -> [b] -> ShowS
showsPrecB p [] = showString "1"
showsPrecB p [b] = showsPrec p b
showsPrecB p bs = showParen (p > prec) . foldr (.) id
                  . intersperse (showString " \\wedge ")
                  . map (showsPrec prec) $ bs
  where prec = 6

instance Show b => Show (Basis b) where
  showsPrec p = showsPrecB p . unpack

-- normalize xs sorts xs; if any elements are repeated, it returns Nothing; it
-- also returns the parity of the list
normalize :: Ord b => [b] -> Maybe (Z2, [b])
normalize [] = Just (0, [])
normalize (x:xs) = do
  (c1, xs') <- normalize xs
  (c2, xxs') <- insert x xs'
  return (c1+c2, xxs')
  where
    -- insert x ys  inserts x into ys, keeping track of parity and duplication
    insert x [] = Just (0, [x])
    insert x yys@(y:ys) = case compare x y of
      LT -> Just (0, x:yys)
      EQ -> Nothing
      GT -> do
        (c, xys) <- insert x ys
        return (c+1, y:xys)

-- version of normalize for passing to returnList etc.
normalize' :: (Num r, Ord b) => [b] -> [(Basis b, r)]
normalize' xs = case normalize xs of
  Nothing -> []
  Just (0, xs') -> [(pack xs', 1)]
  Just (_, xs') -> [(pack xs', -1)]

instance (Num r, Ord b) => Multiplicative r (Basis b) where
  one = [(pack [], 1)]
  mul x y = normalize' $ unpack x ++ unpack y

-- For ExteriorAlgebra r b to have a computable grading, b must be
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
        guard (not $ b `elem` bs)
        return $ insert b bs

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
        guard (not $ b `elem` bs)
        return $ insert b bs

type FreeExteriorAlgebra r b = FreeModule r (Basis b)
type ExteriorAlgebra m = FreeExteriorAlgebra (URing m) (UBasis m)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = returnList . normalize' . unpack . fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f . unpack

-- ExteriorAlgebra r b is a free exterior r-algebra

-- The exterior algebra is the quotient of the tensor algebra by the ideal
-- generated by elements of the form a⊗a. Sq^k on an element of this form
-- is a sum of such elements, so the diagonal action on the tensor algebra
-- descends to the exterior algebra.

instance (Ord b, Show b, AModule b) => AModule (Basis b) where
  sq' n = diag n . unpack
