{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Steenrod(Basis(), Steenrod, excess, sq) where
import Z2
import Util
import Algebra
import qualified Restricted as R
import qualified TensorAlgebra as T
import Data.List(partition, foldl')
import Control.Arrow(first)
import Control.Monad(liftM2)
import Grading

-- include only admissible sequences
type B = [Integer]

showB :: B -> String
showB [] = ""
showB [n] = "Sq^{" ++ show n ++ "}"
showB (n:x) = "Sq^{" ++ show n ++ "} " ++ showB x

admissible :: B -> Bool
admissible [] = True
admissible [r] = r > 0
admissible (r:sx@(s:_)) | r >= 2*s  = admissible sx
                        | otherwise = False

excess' :: B -> Integer
excess' [] = 0
excess' [r] = r
excess' (r:sx@(s:x)) = r - 2*s + excess' sx

-- normalize takes a monomial to the equivalent sum of admissible monomials
normalize :: B -> [B]
normalize x = n' [] $ [x] where
  -- this should terminate according to Hatcher (p. 499)
  n' as [] = as
  n' as is = let (as', is') = partition admissible is
             in n' (as ++ as') $ concatMap relation is'
  -- relation x applies one Adem relation to x; it should only be applied to
  -- elements for which a relation actually applies
  relation :: B -> [B]
  relation [0] = [[]]
  relation (r:sx@(s:x)) | r < 2*s   = map (++ x) $ adem r s
                        | otherwise = map (r:) $ relation sx
  adem r s = [ [r+s-t, t] | t <- [0..r `div` 2], odd $ choose (s-t-1) (r-2*t) ]

mul' :: B -> B -> [(B, Z2)]
mul' x y = map (\b -> (b, 1)) $ normalize $ x ++ y

one' :: [(B, Z2)]
one' = [([], 1)]

--------------------------------------------------------------------------------
-- user-visible stuff

newtype Basis = Basis { unBasis :: B } deriving (Eq, Ord)

-- This could be constructed as a quotient of a tensor algebra, but it seems
-- like more trouble than it's worth
type Steenrod = FreeModule Z2 Basis

instance Show Basis where
  show = showB . unBasis

excess :: Basis -> Integer
excess = excess' . unBasis

instance Multiplicative Z2 Basis where
  one = map (first Basis) one'
  mul (Basis x) (Basis y) = map (first Basis) $ mul' x y

-- A convenient way to define/use an A-module
-- Note: any A-module is free over Z/2
class Free Z2 m b => AModule m b where
  -- since sq' could be called by anyone, implementors should check that the
  -- integer argument is positive
  sq' :: Integer -> b -> m

sq :: AModule m b => Integer -> m -> m
sq = free . sq'

instance AModule m b => Module Steenrod m where
  r .* x = free (foldr sq x . unBasis) r
  
instance AModule Steenrod (Basis) where
  sq' r x = case compare r 0 of
    GT -> inject (Basis [r]) * inject x
    EQ -> inject x
    LT -> 0

-- Steenrod squares with lower indices
sq_ :: (AModule m b, Graded b) => Integer -> m -> m
sq_ n x = free (\b -> sq' (n + degree b) b) x

-- The diagonal action on the tensor algebra
square :: AModule m b => Integer -> [b] -> T.TensorAlgebra Z2 b
square n [] | n == 0    = 1
            | otherwise = 0 -- Sq^n 1 = 0 if n /= 0
square n (x:xs) = sum [ sq' t x * square (n-t) xs | t <- [0..n] ]

instance AModule m b => AModule (T.TensorAlgebra Z2 b) (T.Basis b) where
  sq' n = square n . T.unBasis
