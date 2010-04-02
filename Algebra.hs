{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- rings which are free modules over some other ring
module Algebra(module FreeModule, Multiplicative(..)) where
import qualified Restricted as R
import FreeModule

-- Multiplication on the basis induces multiplication on the module
class Multiplicative r b where
  one :: [(b, r)] -- the multiplicative identity; usually of the form [(b, 1)]
  mul :: b -> b -> [(b, r)]
-- This could alternately be
--   one :: FreeModule r b
--   mul :: b -> b -> FreeModule r b
-- but I think this is easier to program with

-- need to use scoped type variables to make ghc resolve the right class
fromInteger' :: forall r b. (Num r, Ord b, Multiplicative r b) =>
  Integer -> FreeModule r b
fromInteger' n = (fromInteger n :: r) .* returnList one

-- the ring structure induced by multiplication on the basis
instance (Num r, Show b, Ord b, Multiplicative r b) => Num (FreeModule r b)
  where
  (+) = (.+.)
  negate = negateA
  x * y = x R.>>= \bx -> y R.>>= \by -> returnList $ bx `mul` by
  fromInteger = fromInteger'
  abs = error "undefined"
  signum = error "undefined"
