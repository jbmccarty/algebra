{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DyerLashof(DyerLashof) where
import Data.List(intersperse)
import ExteriorAlgebra
import Z2
import Grading
import qualified Data.Set as S
import Control.Monad(guard)
import FreeModule
import Steenrod
import TensorAlgebra(diag')
import qualified Restricted as R
import Util(choose)

-- If b is a basis for H^*(X), then Basis b is a multiplicative basis for
-- H^*(D_{\infty, *} X).
type B b = ([Integer], b)
-- the list of integers should be nondecreasing

showsPrecB :: Show b => Int -> B b -> ShowS
showsPrecB p ([], b) = showsPrec p b
showsPrecB p (rs, b) = foldr (.) (showParen True $ showsPrec 0 b)
  . intersperse (showString " ")
  . map (\r -> showString "Q_{" . showsPrec 0 r . showString "}") $ rs

newtype Basis b = Basis { unBasis :: B b } deriving (Eq, Ord)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

instance Show b => Show (Basis b) where
  showsPrec p = showsPrecB p . unpack

instance R.MonadR Basis b where
  return b = Basis ([], b)

-- b should be positively graded
instance (Ord b, Graded b) => Bigraded (Basis b) where
  bidegree (Basis (rs, b)) = (2^(length rs), foldr f (degree b) rs)
    where f r d = r+2*d
  bigrading = S.fromList . map pack . g where
    g (i, j) | i < 1 || j < i = []
    g (1, j) = map (\b -> ([], b)) . S.toList . grading $ j
    g (i, j) | i `mod` 2 /= 0 = []
    g (i, j) = do
      r <- [0..(j-i)]
      guard (((j - r) `mod` 2) == 0)
      (rs, b) <- g (i `div` 2, (j - r) `div` 2)
      guard (admissible r rs)
      return (r:rs, b)
    admissible _ [] = True
    admissible r (r':_) = r <= r'

--instance (Ord b, AModule b) => AModule (Basis b) where
--  sq' i (Basis ([], x)) = freeM (\y -> inject $ Basis ([], y)) $ sq' i x
--  sq' i (Basis (r:rs, x)) = ...

q' :: Integer -> Basis b -> FreeModule Z2 (Basis b)
q' n = undefined -- normalize sequences

q :: Integer -> FreeDyerLashof b -> FreeDyerLashof b
q n = undefined -- normalize sequences, distribute sums and products appropriately...

-- Kludgy
type FreeDyerLashof b = FreeExteriorAlgebra Z2 (Basis b)
type DyerLashof m = FreeDyerLashof (UBasis m)

instance (Ord b, Show b, AModule b, Graded b) => AModule (ExteriorAlgebraBasis (Basis b)) where
  sq' n b = diag' s n $ viewBasis b where
    s n (Basis ([], b)) = include . include $ sq' n b
    s n (Basis (r:rs, b)) = if r > 0 then s1 else s1 + s2 where
      s1 = sum [ fromInteger (choose (d+r-t) (n - 2*t))
                 * q (r + n - 2*t) (s t (Basis (rs, b))) | t <- [0..n `div` 2] ]
      s2 = sum [ s t (Basis (rs, b)) * s (n-t) (Basis (rs, b))
                 | t <- [0..(n-1) `div` 2] ]
      d = degree b
