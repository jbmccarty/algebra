{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Module(module AbelianGroup, Module(..), Free(..)) where
import AbelianGroup

infixr 8 .*
class (Num r, AbelianGroup m) => Module r m where
  -- For a ring r, an r-module m is an abelian group with an operation
  (.*) :: r -> m -> m
  -- such that:
  -- 1 .* x == x
  -- (r * s) .* x == r.*(s.*x)
  -- (r + s) .* x == r .* x + s .* x
  -- r .* (x + y) == r .* x + r .* y

class Module r m => Free r m b | m b -> r where
  -- An r-module m is free with basis b if it has functions
  inject :: b -> m
  free :: Module r n => (b -> n) -> m -> n
  -- such that for any r-module n and any set function f :: b -> n, there
  -- exists a unique r-module morphism free f :: m -> n with
  -- f == free f . inject.
