{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Matrix where
import qualified Data.Vector as V
import Control.Monad.State

-- A type for m x n matrices (m rows, n columns, indexed starting at 0)
type Matrix r = V.Vector (V.Vector r)

-- swap two entries of a vector
swap :: Int -> Int -> V.Vector a -> V.Vector a
swap i j x | i == j = x
           | otherwise = x V.// [(i, x V.! j), (j, x V.! i)]

-- swap two rows of a matrix
swapRows' :: Int -> Int -> Matrix r -> Matrix r
swapRows' = swap

-- swap two columns of a matrix
swapCols' :: Int -> Int -> Matrix r -> Matrix r
swapCols' i j | i == j = id
              | otherwise = V.map (swap i j)

-- linRow' a i j adds a times row i to row j
linRow' :: Num r => r -> Int -> Int -> Matrix r -> Matrix r
linRow' a i j m | i == j = error "linRow': can't add a row to itself"
                | otherwise = m V.// [(j, V.zipWith f (m V.! i) (m V.! j))]
  where f x y = a*x + y

-- linCol' a i j adds a times column i to column j
linCol' :: Num r => r -> Int -> Int -> Matrix r -> Matrix r
linCol' a i j | i == j = error "linCol': can't add a column to itself"
              | otherwise = V.map f where
  f row = row V.// [(j, a*(row V.! i) + (row V.! j))]

-- arbRow' M i j applies the 2x2 matrix M to the ith and jth rows
arbRow' :: Num r => ((r,r),(r,r)) -> Int -> Int -> Matrix r -> Matrix r
arbRow' ((a,b),(c,d)) i j m | i == j = error "arbRow': rows must be distinct"
                            | otherwise = m V.// [(i, V.zipWith f ri rj),
                                                  (j, V.zipWith g ri rj)]
  where ri = m V.! i
        rj = m V.! j
        f x y = a*x + b*y
        g x y = c*x + d*y

-- arbCol' M i j applies the 2x2 matrix M to the ith and jth columns (i.e.
-- multiplication on the right)
arbCol' :: Num r => ((r,r),(r,r)) -> Int -> Int -> Matrix r -> Matrix r
arbCol' ((a,b),(c,d)) i j | i == j = error "arbCol': columns must be distinct"
                            | otherwise = V.map f where
  f row = row V.// [(i, a*ri + c*rj), (j, b*ri + d*rj)]
    where ri = row V.! i
          rj = row V.! j

-- typeclass for principal ideal domains
class Num r => PID r where
  -- If a = b⋅c, then divide a b = c
  divide :: r -> r -> Maybe r
  -- if d = gcd(a, b) = α⋅a + β⋅b, then gcdExpr a b = (α, β)
  gcdExpr :: r -> r -> (r, r)

-- a matrix container that keeps track of row and column operations
newtype MatrixM r a = MatrixM { unMatrixM :: State (Matrix r, Matrix r, Matrix r) a } deriving Monad

pack = MatrixM
unpack = unMatrixM

-- If runM f M = (A, B, C), then A and C are invertible, and M = ABC.
runM :: Num r => MatrixM r a -> Matrix r -> (Matrix r, Matrix r, Matrix r)
runM f m = execState (unpack f) (identity rows, m, identity cols) where
  rows = V.length m
  cols = if rows > 0 then V.length $ V.head m else 0
  identity i = V.fromList . map V.fromList $ id i
  id 0 = []
  id m = (1:replicate (m-1) 0):(map (0:) $ id (m-1))

getM :: MatrixM r (Matrix r)
getM = pack $ do
  (_, b, _) <- get
  return b

swapRows :: Int -> Int -> MatrixM r ()
swapRows i j = pack $ do
  (a, b, c) <- get
  put (swapCols' i j a, swapRows' i j b, c)

swapCols :: Int -> Int -> MatrixM r ()
swapCols i j = pack $ do
  (a, b, c) <- get
  put (a, swapCols' i j b, swapRows' i j c)

linRow :: Num r => r -> Int -> Int -> MatrixM r ()
linRow r i j = pack $ do
  (a, b, c) <- get
  put (linCol' (-r) j i a, linRow' r i j b, c)

linCol :: Num r => r -> Int -> Int -> MatrixM r ()
linCol r i j = pack $ do
  (a, b, c) <- get
  put (a, linCol' r i j b, linRow' (-r) j i c)

arbRow :: Num r => ((r,r),(r,r)) -> Int -> Int -> MatrixM r ()
arbRow ((a,b),(c,d)) i j | a*d-b*c /= 1 = error "arbRow: determinant must be 1"
                         | otherwise = pack $ do
  (a', b', c') <- get
  put (arbCol' ((d,-b),(-c,a)) i j a', arbRow' ((a,b),(c,d)) i j b', c')

arbCol :: Num r => ((r,r),(r,r)) -> Int -> Int -> MatrixM r ()
arbCol ((a,b),(c,d)) i j | a*d-b*c /= 1 = error "arbCol: determinant must be 1"
                         | otherwise = pack $ do
  (a', b', c') <- get
  put (a', arbCol' ((a,b),(c,d)) i j b', arbRow' ((d,-b),(-c,a)) i j c')

test :: (Matrix Integer, Matrix Integer, Matrix Integer)
test = runM f . V.fromList . map V.fromList $ [[1,2,3],[4,5,6],[7,8,9]] where
  f = do
    linRow (-4) 0 1
    linRow (-7) 0 2
    linRow (-2) 1 2
    linCol (-2) 0 1
    linCol (-3) 0 2
    linCol (-2) 1 2



-- calculate the Smith normal form of a matrix
-- If smith M = (A, B, C), then M = ABC, B is in Smith normal form, and A and C
-- are invertible.
--smith :: PID r => Matrix r -> (Matrix r, Matrix r, Matrix r)
--smith m = maybe findNonZero 

-- put a m x n matrix in row-reduced echelon form
-- Note: this doesn't work well for floats due to round-off error
rref :: Fractional r => Matrix r -> Matrix r
rref m = rref' 0 0 m where
  rows = V.length m
  cols = if rows > 0 then V.length $ V.head m else 0
  -- rref' i j assumes that the matrix is already in rref above row i and to
  -- the left of column j
  rref' i j x | i >= rows || j >= cols = x
              | otherwise = maybe (rref' i (j+1) x)
                            (rref' (i+1) (j+1) . doRow . (i+))
                            . V.findIndex ((0 /=) . (V.! j)) . V.drop i $ x
    where doRow k = elimOther . normalize . swap i k $ x
          normalize y = y V.// [(i, V.map (/ (y V.! i V.! j)) $ y V.! i)]
          elimOther y = V.imap f y where
            f k row | k == i = row
                    | otherwise = V.zipWith (g $ row V.! j) row $ y V.! i
            g e r1 r2 = r1 - e*r2
  swap i j x | i == j = x
             | otherwise = x V.// [(i, x V.! j), (j, x V.! i)]
