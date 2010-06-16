{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KZ2(module Steenrod, KZ2(), iota) where
import Steenrod
import Algebra
import SymmetricAlgebra
import Z2
import qualified Restricted as R
import Data.TypeLevel(Nat, toNum)
-- from the type-level package in hackage
import Control.Arrow(first)
import Basis
import Grading
import qualified Data.Set as S

newtype KZ2B n = KZ2B { unKZ2B :: UBasis Steenrod } deriving (Eq, Ord)

-- NatI n
-- use toNum
instance Nat n => Show (KZ2B n) where
  show (KZ2B xs :: KZ2B n) = show xs ++ "_{"
                             ++ show (toNum (undefined :: n)) ++ "}"

instance Nat n => AModule (KZ2B n) where
  sq' r (KZ2B x :: KZ2B n) = freeM filterExcess $ sq' r x where
    filterExcess b = if excess b < n' then inject $ KZ2B b else zero
    n' = toNum (undefined :: n)

instance Nat n => Graded (KZ2B n) where
  degree (b :: KZ2B n) = (+ n') . degree . unKZ2B $ b where
    n' = toNum (undefined :: n)
  grading = S.map KZ2B . S.filter ((< n') . excess) . grading . (subtract n')
    :: Integer -> S.Set (KZ2B n) where
    n' = toNum (undefined :: n)

type KZ2 n = SymmetricAlgebra Z2 (KZ2B n)

iota :: Nat n => n -> KZ2 n
iota _ = 1 R.>>= (injectB . KZ2B)
