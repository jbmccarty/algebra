{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KZ2(module Steenrod, KZ2(), iota) where
import Steenrod
import Algebra
import SymmetricAlgebra
import Z2
import qualified Restricted as R
import Natural
-- from the type-level package in hackage
import Control.Arrow(first)
import Basis
import Grading
import qualified Data.Set as S

newtype KZ2B n = KZ2B { unKZ2B :: UBasis Steenrod } deriving (Eq, Ord)

instance Nat n => Show (KZ2B n) where
  showsPrec p (KZ2B xs :: KZ2B n) = showParen (p > prec && degree xs > 0) $
    showsPrec 0 xs . showString "_{" . showsPrec 0 (toNum (undefined :: n))
    . showString "}"
    where prec = 5

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

type KZ2 n = FreeSymmetricAlgebra Z2 (KZ2B n)

iota :: Nat n => n -> KZ2 n
iota _ = 1 R.>>= (injectB . KZ2B)
