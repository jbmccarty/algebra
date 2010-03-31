{-# LANGUAGE MultiParamTypeClasses #-}

module Module(module AbelianGroup, Module(..)) where
import AbelianGroup

class (Num r, AbelianGroup m) => Module r m where
  (!) :: r -> m -> m

infixr 8 !
