{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExteriorAlgebra(module Algebra, ExteriorAlgebra()) where
import Algebra
import Z2
import qualified Restricted as R
import qualified Data.Map as M
import Basis

-- The exterior algebra could be implemented as a quotient of the tensor
-- algebra, but I would prefer to have a canonical basis.

-- should always be sorted and include no duplicates
newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

showB :: Show b => [b] -> String
showB [] = "1"
showB [b] = show b
showB (b:x) = show b ++ " \\wedge " ++ showB x

instance Show b => Show (Basis b) where
  show = showB . unpack

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

type ExteriorAlgebra r b = FreeModule r (Basis b)

instance Ord b' => BasisFunctor Basis b b' where
  fmapB' f = returnList . normalize' . unpack . fmap f

instance (Ord b', Show b') => BasisMonad Basis b b' where
  bindB' f = product . map f . unpack

-- ExteriorAlgebra r b is a free exterior r-algebra
