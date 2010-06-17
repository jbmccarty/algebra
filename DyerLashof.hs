module DyerLashof(DyerLashof) where
import Data.List(intersperse)
import ExteriorAlgebra
import Z2
import Grading
import qualified Data.Set as S
import Control.Monad(guard)
import FreeModule

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

-- Kludgy
type FreeDyerLashof b = FreeExteriorAlgebra Z2 (Basis b)
type DyerLashof m = FreeDyerLashof (UBasis m)
