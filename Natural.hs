{-# LANGUAGE
  RankNTypes
  #-}
module Natural(module GHC.TypeLits, module Data.Proxy, useNatural) where
import GHC.TypeLits
import Data.Proxy

useNatural :: (forall n . KnownNat n => Proxy n -> a) -> Integer -> a
useNatural f n = case someNatVal n of
  Just (SomeNat p) -> f p
  Nothing -> error $ "useNatural: " ++ show n ++ " is not a natural number"
