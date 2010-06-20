{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DyerLashof(FreeDyerLashof, DyerLashof, q) where
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
import Range

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

instance ViewBasis (Basis b) (B b) where
  viewBasis = unpack

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

-- Calculate Q_r of a sequence of Q's. This should terminate since it reduces
-- the lexicographic ordering of each term.
q' :: Ord b => Integer -> Basis b -> FreeModule Z2 (Basis b)
q' n (Basis ([], b)) = inject $ Basis ([n], b)
q' n (Basis (rrs@(r:rs), b))
  | n <= r = inject $ Basis (n:rrs, b)
  | otherwise =
    foldr (.+.) zero [ freeM (q' (e-2*d+r)) $ q' (d+r) $ Basis (rs, b)
                       | d <- range (Inc $ e%3, Exc $ e%2),
                       odd $ choose (e-d-1) d ]
  where e = n-r
  -- this shouldn't ever have Q_0 being applied to a sum, so I don't need
  -- multiplication

q :: (Ord b, Show b) => Integer -> FreeDyerLashof b -> FreeDyerLashof b
q r x = if r > 0 then s1 else s1+s2 where
  s1 = freeM f x -- Q_r is additive unless r = 0
  f b = case viewBasis b of
    [] -> if r == 0 then 1 else 0 -- Q_r(1) = 0 iff r > 0
    [x] -> include $ q' r x -- no products involved
    _ -> 0 -- Q_r of a nontrivial product is zero
  s2 = p . map (returnList . (:[])) . viewFM $ x -- s2 is the "half-square" of x
  p [] = 0
  p (t:ts) = sum (map (t*) ts) + p ts

-- Kludgy
type FreeDyerLashof b = FreeExteriorAlgebra Z2 (Basis b)
type DyerLashof m = FreeDyerLashof (UBasis m)

instance (Ord b, Show b, AModule b, Graded b) => AModule (ExteriorAlgebraBasis (Basis b)) where
  sq' n b = diag' s n $ viewBasis b where
    s n (Basis ([], b)) = include . include $ sq' n b
    s n (Basis (r:rs, b)) = if r > 0 then s1 else s1 + s2 where
      s1 = sum [ q (r + n - 2*t) (s t (Basis (rs, b)))
                 | t <- range (Inc 0, Inc $ n%2),
                 odd $ choose (d+r-t) (n - 2*t) ]
      s2 = sum [ s t (Basis (rs, b)) * s (n-t) (Basis (rs, b))
                 | t <- range (Inc 0, Exc $ n%2) ]
      d = degree b
