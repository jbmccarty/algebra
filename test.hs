{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import FreeModule
import TensorAlgebra
import SymmetricAlgebra
import ExteriorAlgebra
import Z2
import qualified Restricted as R

data Basis = A | B deriving (Eq, Ord, Show)

a :: R.MonadR m Basis => m Basis
a = R.return A

b :: R.MonadR m Basis => m Basis
b = R.return B

main = do
  print (a .+. b .+. a :: FreeModule Integer Basis)
  print (a .+. b .+. a :: FreeModule Z2 Basis)
  print (a + 3 * b * (a + b) :: TensorAlgebra Integer Basis)
  print (a + 3 * b * (a + b) :: TensorAlgebra Z2 Basis)
  print (a + 3 * b * (a + b) :: SymmetricAlgebra Integer Basis)
  print (a + 3 * b * (a + b) :: SymmetricAlgebra Z2 Basis)
  print (a + 3 * b * (a + b) :: ExteriorAlgebra Integer Basis)
  print (a + 3 * b * (a + b) :: ExteriorAlgebra Z2 Basis)

{-
import Steenrod
import KZ2
import Natural

stuff :: Integer -> String -> String
stuff n = useNatural f n where
  f m = unlines . map (show . foldr sq (iota m) . map read . words) . lines

main = do
  l <- getLine
  let n = head . map read . words $ l
  interact (stuff n)
-}
