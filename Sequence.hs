module Sequence where
import DyerLashof
import FreeModule
import Basis
import Steenrod
import Grading
import ExteriorAlgebra
import Data.List(genericLength)

-- differential n is the 2^n differential of the spectral sequence
differential :: (Ord b, Show b, Graded b, AModule b) =>
  Integer -> FreeDyerLashof b -> FreeDyerLashof b
differential n = freeM (f . viewBasis) where
  f [] = 0
  f (r:rs) = (include . inject) r * f rs
             + d n (viewBasis r) * product (map (include . inject) rs)
  d m ([], b) = 0
  d m (r:rs, b) = case compare m (genericLength rs) of
    GT -> 0
    EQ -> sq_ (r+1) x + if r > 0 && m > 0 then q (r-1) (d (m-1) (rs, b)) else 0
    LT -> if r == 0 then x * d m (rs, b) else 0
    where x = foldr q (include . include . inject $ b) rs
-- all the conversion back and forth is really inefficient
