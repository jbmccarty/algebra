{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Module(module AbelianGroup, Module(..)) where
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
