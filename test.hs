{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import FreeModule
import TensorAlgebra
import SymmetricAlgebra
import ExteriorAlgebra
import Z2
import qualified Restricted as R
import Basis
import Steenrod
import KZ2
import Natural
import Grading
import qualified Data.Set as S
import DyerLashof
import Suspension
import Sequence

data Basis = A | B deriving (Eq, Ord, Show)

type ZCoef = FreeModule Integer Basis
type Z2Coef = FreeModule Z2 Basis

a = R.return A
a' = injectB A
b = R.return B
b' = injectB B

i = include . include $ 1 :: DyerLashof Steenrod

type Foo = DyerLashof Z2Coef

stuff :: Integer -> String -> String
stuff n = useNatural f n where
  f m = unlines . map (show . foldr sq (iota m) . map read . words) . lines

main = do
{-
  print (a .+. b .+. a :: ZCoef)
  print (a .+. b .+. a :: Z2Coef)
  print (a' + 3 * b' * (a' + b') :: TensorAlgebra ZCoef)
  print (a' + 3 * b' * (a' + b') :: TensorAlgebra Z2Coef)
  print (a' + 3 * b' * (a' + b') :: SymmetricAlgebra ZCoef)
  print (a' + 3 * b' * (a' + b') :: SymmetricAlgebra Z2Coef)
  print (a' + 3 * b' * (a' + b') :: ExteriorAlgebra ZCoef)
  print (a' + 3 * b' * (a' + b') :: ExteriorAlgebra Z2Coef)
  print (grading 9 :: S.Set (UBasis Steenrod))
  print $ sq 2 ((iota d6)^3)
  print $ sq_ (-16) ((iota d6)^3)

  print (bigrading (4, 6) :: S.Set (UBasis (DyerLashof (Suspend D1 Steenrod))))
  print $ q 0 (sq 3 i + sq 2 i + i*sq 4 i)
-}
  putStr $ concat [ show x ++ " & " ++ show (differential 1 x) ++ "\\\\\n" | x <- map inject . S.toList $ bigrading (4, 7) :: [DyerLashof (Suspend D1 Steenrod)] ]

{-
  l <- getLine
  let n = head . map read . words $ l
  interact (stuff n)
-}
