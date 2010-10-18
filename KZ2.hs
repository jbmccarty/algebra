{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module KZ2(module Steenrod, KZ2(), iota) where
import Steenrod
import Algebra
import SymmetricAlgebra hiding (Basis)
import qualified SymmetricAlgebra as SA
import TensorAlgebra(diag')
import Z2
import qualified Restricted as R
import Natural
import Control.Arrow(first)
import Basis
import Grading
import qualified Data.Set as S

newtype KZ2B n = KZ2B { unKZ2B :: UBasis Steenrod } deriving (Eq, Ord)

instance Nat n => Show (KZ2B n) where
  showsPrec p (KZ2B xs :: KZ2B n) = showParen (p > prec && degree xs > 0) $
    showsPrec 0 xs . showString "_{" . showsPrec 0 (toNum (undefined :: n))
    . showString "}"
    where prec = 7

instance Nat n => Graded (KZ2B n) where
  degree (b :: KZ2B n) = (+ n') . degree . unKZ2B $ b where
    n' = toNum (undefined :: n)
  grading = S.map KZ2B . S.filter ((< n') . excess) . grading . (subtract n')
    :: Integer -> S.Set (KZ2B n) where
    n' = toNum (undefined :: n)

type KZ2 n = FreeSymmetricAlgebra Z2 (KZ2B n)

-- this overlaps with the default SymmetricAlgebra instance
-- I should use a newtype instead
instance Nat n => AModule (SA.Basis (KZ2B n)) where
  sq' t (x :: SA.Basis (KZ2B n)) = diag' s t (viewBasis x) where
    s r b@(KZ2B b') = case compare r (degree b) of
      LT -> include . freeM filterExcess $ sq' r b'
      EQ -> b'' * b'' where b'' = include . inject $ b
      GT -> 0 -- this could be combined with the LT case, but this is faster
    filterExcess b = if excess b < n' then inject $ KZ2B b else zero
    n' = toNum (undefined :: n)

iota :: Nat n => n -> KZ2 n
iota _ = 1 R.>>= (injectB . KZ2B)
