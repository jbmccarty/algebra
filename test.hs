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

data Basis = A | B deriving (Eq, Ord, Show)

a = R.return A
a' = injectB A
b = R.return B
b' = injectB B

stuff :: Integer -> String -> String
stuff n = useNatural f n where
  f m = unlines . map (show . foldr sq (iota m) . map read . words) . lines

main = do
  print (a .+. b .+. a :: FreeModule Integer Basis)
  print (a .+. b .+. a :: FreeModule Z2 Basis)
  print (a' + 3 * b' * (a' + b') :: TensorAlgebra Integer Basis)
  print (a' + 3 * b' * (a' + b') :: TensorAlgebra Z2 Basis)
  print (a' + 3 * b' * (a' + b') :: SymmetricAlgebra Integer Basis)
  print (a' + 3 * b' * (a' + b') :: SymmetricAlgebra Z2 Basis)
  print (a' + 3 * b' * (a' + b') :: ExteriorAlgebra Integer Basis)
  print (a' + 3 * b' * (a' + b') :: ExteriorAlgebra Z2 Basis)
  print (grading 9 :: S.Set (UBasis Steenrod))
  print $ sq 2 ((iota d6)^3)
  print $ sq_ (-16) ((iota d6)^3)

  l <- getLine
  let n = head . map read . words $ l
  interact (stuff n)
