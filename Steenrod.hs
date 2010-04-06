{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Steenrod(Steenrod, SteenrodBasis, excess, sq, AModule(..), sq_) where
import Z2
import Util
import Algebra
import qualified Restricted as R
import Data.List(partition, foldl')
import Control.Arrow(first)
import Control.Monad(liftM2)
import Grading
import Basis

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

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

type SteenrodBasis = Basis

-- This could be constructed as a quotient of a tensor algebra, but it seems
-- like more trouble than it's worth
type Steenrod = FreeModule Z2 Basis

instance Show Basis where
  show = showB . unpack

excess :: Basis -> Integer
excess = excess' . unpack

instance Multiplicative Z2 Basis where
  one = map (first pack) one'
  mul (Basis x) (Basis y) = map (first Basis) $ mul' x y

-- Conversely, the action of a single square on an A-module.
sq :: Module Steenrod m => Integer -> m -> m
sq n m = case compare n 0 of
  GT -> (inject (pack [n]) :: Steenrod) .* m
  EQ -> m
  LT -> zero

-- A convenient way to define the A-module structure on a free Z/2-module.
-- Note: every A-module is free over Z/2.
class AModule b where
  -- since sq' could be called by anyone, implementors should check that the
  -- integer argument is positive
  sq' :: Integer -> b -> FreeModule Z2 b

instance (Ord b, AModule b) => Module Steenrod (FreeModule Z2 b) where
  s .* x = freeM (foldr (freeM . sq') x . unpack) s

instance AModule Basis where
  sq' r x = case compare r 0 of
    GT -> inject (pack [r]) * inject x
    EQ -> inject x
    LT -> 0

-- Steenrod squares with lower indices
sq_ :: (Ord b, AModule b, Graded b)
  => Integer -> (FreeModule Z2 b) -> FreeModule Z2 b
sq_ n = freeM (\b -> sq' (n + degree b) b)

-- The diagonal action on the tensor algebra
square :: (Ord b, Show b, AModule b) => Integer -> [b] -> TensorAlgebra Z2 b
square n [] | n == 0    = 1
            | otherwise = 0 -- Sq^n 1 = 0 if n /= 0
square n (x:xs) = sum [ include (sq' t x) * square (n-t) xs | t <- [0..n] ]

instance (Ord b, Show b, AModule b) => AModule (TensorAlgebraBasis b) where
  sq' n = square n . unpackTAB
