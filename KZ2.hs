{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KZ2(KZ2(), iota) where
import Steenrod
import Algebra
import SymmetricAlgebra
import Z2
import qualified Restricted as R
import Data.TypeLevel(Nat, toNum)
-- from the type-level package in hackage
import Control.Arrow(first)
import Basis

newtype KZ2B n = KZ2B { unKZ2B :: SteenrodBasis } deriving (Eq, Ord)

-- NatI n
-- use toNum
instance Nat n => Show (KZ2B n) where
  show (KZ2B xs :: KZ2B n) = show xs ++ " \\iota_{"
                             ++ show (toNum (undefined :: n)) ++ "}"

instance Nat n => AModule (KZ2B n) where
  sq' r (KZ2B x :: KZ2B n) = freeM filterExcess $ sq' r x where
    filterExcess b = if excess b < n' then inject $ KZ2B b else zero
    n' = toNum (undefined :: n)

type KZ2 n = SymmetricAlgebra Z2 (KZ2B n)

iota :: Nat n => n -> KZ2 n
iota _ = 1 R.>>= (injectB . KZ2B)
