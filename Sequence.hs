module Sequence where
import DyerLashof
import FreeModule
import Basis
import Steenrod
import Grading
import ExteriorAlgebra

-- the first differential of the spectral sequence
differential :: (Ord b, Show b, Graded b, AModule b) =>
  FreeDyerLashof b -> FreeDyerLashof b
differential = freeM (f . viewBasis) where
  f [] = 0
  f (r:rs) = (include . inject) r * f rs
             + d (viewBasis r) * product (map (include . inject) rs)
  d ([], b) = 0
  d ([r], b) = include . include $ sq_ (r+1) (inject b)
  d (0:rs, b) = foldr q (include . include . inject $ b) rs * d (rs, b)
  d _ = 0
-- all the conversion back and forth is really inefficient
