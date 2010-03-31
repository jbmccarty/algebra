module Grading(Graded(..)) where
import qualified Data.Set as S

class Graded b where
  degree :: b -> Integer -- the degree of a basis element
  grading :: Integer -> S.Set b -- the set of all elements of a given degree

-- If a basis b is Graded and Multiplicative over a ring r, then
-- one :: [(b, r)] should be homogeneous of degree 0, and for any basis
-- elements x and y, mul x y :: [(b, r)] should be homogeneous of degree
-- |x| + |y|.
