{-# LANGUAGE RankNTypes #-}

module Natural(Nat, toNum, D0, D1, D2, D3, D4, D5, D6, D7, D8, D9,
  d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, (:*)(..), useNatural)
where
import Data.TypeLevel

useNatural :: (forall n. Nat n => n -> a) -> Integer -> a
useNatural f n = reifyIntegral n f
