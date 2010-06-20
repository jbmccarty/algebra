{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Steenrod(Steenrod, excess, sq, AModule(..), sq_) where
import Z2
import Util
import Algebra
import qualified Restricted as R
import Data.List(partition, foldl')
import Control.Arrow(first)
import Control.Monad(liftM2)
import Grading
import Basis
import qualified Data.Set as S
import Range

-- include only admissible sequences
type B = [Integer]

showsPrecB :: Int -> B -> ShowS
showsPrecB p rs = showParen (p > prec && (not . null) rs)
                  . foldr (.) (showString "\\iota") . map s $ rs
  where s r = showString "Sq^{" . showsPrec 0 r . showString "} "
        prec = 5

admissible :: B -> Bool
admissible [] = True
admissible [r] = r > 0
admissible (r:sx@(s:_)) | r >= 2*s  = admissible sx
                        | otherwise = False

topSquare :: B -> Integer
topSquare [] = 0
topSquare (r:_) = r

degree' :: B -> Integer
degree' = sum

grading' :: Integer -> S.Set B
grading' n = case compare n 0 of
  LT -> S.empty
  EQ -> S.singleton []
  GT -> S.fromList [ i:x | j <- range (Inc 0, Inc $ n%2), let i = n-j,
                     x <- S.toList $ grading' j, i - 2*topSquare x >= 0 ]

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
  adem r s = [ [r+s-t, t] | t <- range (Inc 0, Inc $ r%2),
               odd $ choose (s-t-1) (r-2*t) ]

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

-- This could be constructed as a quotient of a tensor algebra, but it seems
-- like more trouble than it's worth
type Steenrod = FreeModule Z2 Basis

instance Show Basis where
  showsPrec p = showsPrecB p . unpack

instance Graded Basis where
  degree = degree' . unpack
  grading = S.map pack . grading'

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
