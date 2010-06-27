{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Matrix where
import qualified Data.Vector as V
import Control.Monad.State
import Data.Maybe(isJust, fromJust)

-- typeclass for integral domains
class Num r => IntegralDomain r where
  -- Divisibility in an integral domain should be decidable: for all nonzero a
  -- and all b in r, there exists at most one c in r such that a⋅c = b. In this
  -- case, a `divides` b = Just c; otherwise, a `divides` b = Nothing.
  -- Note: 0 divides 0, but not uniquely, and 0 does not divide anything else.
  divides :: r -> r -> Maybe r

instance IntegralDomain Integer where
  divides 0 0 = Just 0
  divides 0 _ = Nothing
  divides a b = case b `quotRem` a of
    (q, 0) -> Just q
    _ -> Nothing

-- test whether an element is a unit
isUnit :: IntegralDomain r => r -> Bool
isUnit a = isJust $ a `divides` 1

-- typeclass for principal ideal domains
class IntegralDomain r => PID r where
  -- In a PID, we have gcd(a, b) = α⋅a + β⋅b for some α and β.
  -- gcdExpr a b = (α, β) in this case.
  gcdExpr :: r -> r -> (r, r)

instance PID Integer where
  gcdExpr _ 0 = (1, 0)
  gcdExpr a b = case quotRem a b of
    (q, 0) -> (0, 1) -- if b divides a, then gcd(a, b) = 0a + 1b
    (q, r) -> case gcdExpr b r of
      (alpha, beta) -> (beta, alpha - beta*q)
      -- if a = qb + r, then r = a - qb, so
      -- 1. gcd(b, r) divides a, hence gcd(b, r) divides gcd(a, b)
      -- 2. gcd(a, b) divides a - qb = r, so gcd(a, b) divides gcd(b, r)
      -- 3. so gcd(a, b) = gcd(b, r) up to units
      -- If gcd(b, r) = αb + βr, then gcd(a, b) = gcd(b, r) = αb + βr
      -- = αb + β(a-qb) = βa + (α - βq)b

-- A type for m x n matrices (m rows, n columns, indexed starting at 0)
type Matrix r = V.Vector (V.Vector r)

rows :: Matrix r -> Int
rows = V.length

columns :: Matrix r -> Int
columns x = if V.length x > 0 then V.length $ V.head x else 0

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

-- addRow' a i j adds a times row i to row j
addRow' :: Num r => r -> Int -> Int -> Matrix r -> Matrix r
addRow' a i j m | i == j = error "addRow': can't add a row to itself"
                | otherwise = m V.// [(j, V.zipWith f (m V.! i) (m V.! j))]
  where f x y = a*x + y

-- addCol' a i j adds a times column i to column j
addCol' :: Num r => r -> Int -> Int -> Matrix r -> Matrix r
addCol' a i j | i == j = error "addCol': can't add a column to itself"
              | otherwise = V.map f where
  f row = row V.// [(j, a*(row V.! i) + (row V.! j))]

-- multiply a row by a unit
scaleRow' :: IntegralDomain r => r -> Int -> Matrix r -> Matrix r
scaleRow' a i m | not $ isUnit a = error "scaleRow': can't scale by a non-unit"
                | otherwise = m V.// [(i, V.map (a*) $ m V.! i)]

-- multiply a column by a unit
scaleCol' :: IntegralDomain r => r -> Int -> Matrix r -> Matrix r
scaleCol' a i | not $ isUnit a = error "scaleCol': can't scale by a non-unit"
              | otherwise = V.map f
  where f row = row V.// [(i, a*(row V.! i))]

-- arbRow' M i j applies the 2x2 matrix M to the ith and jth rows
arbRow' :: IntegralDomain r =>
  ((r,r),(r,r)) -> Int -> Int -> Matrix r -> Matrix r
arbRow' ((a,b),(c,d)) i j m
  | i == j = error "arbRow': rows must be distinct"
  | not $ isUnit (a*d-b*c) = error "arbRow': determinant must be a unit"
  | otherwise = m V.// [(i, V.zipWith f ri rj), (j, V.zipWith g ri rj)]
  where ri = m V.! i
        rj = m V.! j
        f x y = a*x + b*y
        g x y = c*x + d*y

-- arbCol' M i j applies the 2x2 matrix M to the ith and jth columns (i.e.
-- multiplication on the right)
arbCol' :: IntegralDomain r =>
  ((r,r),(r,r)) -> Int -> Int -> Matrix r -> Matrix r
arbCol' ((a,b),(c,d)) i j
  | i == j = error "arbCol': columns must be distinct"
  | not $ isUnit (a*d-b*c) = error "arbCol': determinant must be a unit"
  | otherwise = V.map f where
    f row = row V.// [(i, a*ri + c*rj), (j, b*ri + d*rj)]
      where ri = row V.! i
            rj = row V.! j

-- a matrix container that keeps track of row and column operations
newtype MatrixM r a = MatrixM { unMatrixM :: State (Matrix r, Matrix r, Matrix r) a } deriving (Functor, Monad)

pack = MatrixM
unpack = unMatrixM

-- If runM f M = (a, M', R, C), then R and C are invertible, and M' = RMC;
-- "a" will be whatever data the computation returns
runM :: Num r => MatrixM r a -> Matrix r -> (a, Matrix r, Matrix r, Matrix r)
runM f m = case runState (unpack f) (m, identity $ rows m, identity $ columns m)
  of (a, (m', r, c)) -> (a, m', r, c)
  where
    identity i = V.fromList . map V.fromList $ id i
    id 0 = []
    id i = (1:replicate (i-1) 0):(map (0:) $ id (i-1))

getM :: MatrixM r (Matrix r)
getM = pack $ do
  (m, _, _) <- get
  return m

swapRows :: Num r => Int -> Int -> MatrixM r ()
swapRows i j = pack $ do
  (m, r, c) <- get
  put (swapRows' i j m, swapRows' i j r, c)

swapCols :: Num r => Int -> Int -> MatrixM r ()
swapCols i j = pack $ do
  (m, r, c) <- get
  put (swapCols' i j m, r, swapCols' i j c)

addRow :: Num r => r -> Int -> Int -> MatrixM r ()
addRow a i j = pack $ do
  (m, r, c) <- get
  put (addRow' a i j m, addRow' a i j r , c)

addCol :: Num r => r -> Int -> Int -> MatrixM r ()
addCol a i j = pack $ do
  (m, r, c) <- get
  put (addCol' a i j m, r, addCol' a i j c)

scaleRow :: IntegralDomain r => r -> Int -> MatrixM r ()
scaleRow a i = pack $ do
  (m, r, c) <- get
  put (scaleRow' a i m, scaleRow' a i r, c)

scaleCol :: IntegralDomain r => r -> Int -> MatrixM r ()
scaleCol a i = pack $ do
  (m, r, c) <- get
  put (scaleCol' a i m, r, scaleCol' a i c)

arbRow :: IntegralDomain r => ((r,r),(r,r)) -> Int -> Int -> MatrixM r ()
arbRow ((a,b),(c,d)) i j = pack $ do
    (m, r, c') <- get
    put (arbRow' ((a,b),(c,d)) i j m, arbRow' ((a,b),(c,d)) i j r, c')

arbCol :: IntegralDomain r => ((r,r),(r,r)) -> Int -> Int -> MatrixM r ()
arbCol ((a,b),(c,d)) i j = pack $ do
    (m, r, c') <- get
    put (arbCol' ((a,b),(c,d)) i j m, r, arbCol' ((a,b),(c,d)) i j c')

-- put a matrix in upper-triangular form using row operations having
-- determinant ± 1; the Bool value is whether the determinant is positive
semi_rref :: PID r => MatrixM r Bool
semi_rref = s 0 0 where
  s i j = do
    m <- getM
    let r = rows m
        c = columns m
    if i >= r || j >= c then return True else case findNonzeroEntry i j m of
        Nothing -> s i (j+1)
        Just k -> (if k == i then return id else swapRows i k >> return not)
          >>= \f -> do
          -- the above is really contorted: making the Bool part of the state
          -- might be more clear
            elimColumn i r j
            fmap f $ s (i+1) (j+1)

  -- find the first nonzero entry in column j, discounting rows above row i
  findNonzeroEntry i j = fmap (i+) . V.findIndex ((0 /=) . (V.! j)) . V.drop i

  -- eliminate entries in column j and rows (i+1)..r using secondary row
  -- operations
  elimColumn i r j = mapM_ (elimEntry i j) [(i+1)..(r-1)]
  elimEntry i j k = do
    m <- getM
    let a = m V.! i V.! j
        b = m V.! k V.! j
        (alpha, beta) = gcdExpr a b
        d = alpha*a + beta*b -- d = gcd(a, b)
        a' = fromJust $ divides d a -- so d divides a and b
        b' = fromJust $ divides d b
    arbRow ((alpha,beta),(-b',a')) i k
    -- After multiplication by this matrix, m will be gcd(a, b) in position i,
    -- j, and zero in position k, j.
    -- Since d = αa + βb, then 1 = αa/d + βb/d = αa' + βb', which is also the
    -- determinant of this matrix.
    
easy_rref :: (Fractional r, IntegralDomain r) => MatrixM r ()
easy_rref = e 0 0 where
  e i j = do
    m <- getM
    let r = rows m
        c = columns m
    when (i < r && j < c) $ case findNonzeroEntry i j m of
      Nothing -> e i (j+1)
      Just k -> do
        when (k /= i) $ swapRows i k
        scale i j
        elimColumn i r j
        e (i+1) (j+1)

  -- find the first nonzero entry in column j, discounting rows above row i
  findNonzeroEntry i j = fmap (i+) . V.findIndex ((0 /=) . (V.! j)) . V.drop i

  -- scale a row so the leading coefficient is 1
  scale i j = do
    m <- getM
    let a = m V.! i V.! j
    scaleRow (1/a) i

  -- eliminate entries in column j and rows other than i
  elimColumn i r j = mapM_ (elimEntry i j) $ [0..(i-1)] ++ [(i+1)..(r-1)]
  elimEntry i j k = do
    m <- getM
    let a = m V.! k V.! j
    addRow (-a) i k

-- compute the Smith normal form of a matrix
smith :: PID r => MatrixM r ()
smith = s 0 where
  -- after running s i, the matrix is diagonal, but the entries may not form a
  -- divisor chain
  s i = do
    m <- getM
    when (i < rows m && i < columns m) $ case findNonzeroEntry i m of
      Nothing -> return ()
      Just (k, j) -> do
        when (i /= k) $ swapRows i k -- put a nonzero entry in position (i, i)
        when (i /= j) $ swapCols i j
        elimColumn i -- eliminate entries in column i, below row i
        doWhile $ cycle [elimRow i, elimColumn i]
        -- ^ do column and row operations alternately until one of them uses
        -- no secondary column/row operations; this means that row/column i was
        -- not modified by the column/row operations and hence still contains
        -- zeros where required; this is guaranteed to terminate since we are
        -- in a PID
        s (i+1)
        -- now we inductively assume that the submatrix below row i and right
        -- of column i is in Smith normal form, but the (i,i) entry might not
        -- divide the (i+1,i+1) entry
        divisorChain i -- thus we make it so
        
  -- return the indices of a nonzero entry in the matrix, if any
  findNonzeroEntry i m = do
    let m' = V.map (V.findIndex (0 /=) . V.drop i) . V.drop i $ m
    r <- V.findIndex isJust m'
    c <- m' V.! r
    return (i+r, i+c)

  -- doWhile xs performs the list of actions in xs until one of them returns
  -- False
  doWhile :: Monad m => [m Bool] -> m ()
  doWhile = foldr f $ return () where
    f g x = g >>= flip when x

  -- Eliminate entries in the ith column, below row i.
  -- return True if any secondary row operations were used, otherwise return
  -- False
  elimColumn i = do
    m <- getM
    opTypes <- mapM (elimCEntry i) . V.toList . V.map (i+1+)
               . V.findIndices (0 /=) . V.map (V.! i) . V.drop (i+1) $ m
    return $ foldr (||) False opTypes
    -- note that this returns False if nothing at all was done, as it should

  -- Eliminate the entry in row k, column i, using a row operation on rows i
  -- and k. Uses an elementary row operation if possible, otherwise uses a
  -- secondary row operation. Returns True if a secondary row operation was
  -- used.
  -- Note that we _must_ preferentially use elementary operations, otherwise we
  -- may encounter cycles of secondary row/column operations given an unlucky
  -- choice of gcdExpr.
  elimCEntry i k = do
    m <- getM
    let a = m V.! i V.! i
        b = m V.! k V.! i
    case a `divides` b of
      Just q -> addRow (-q) i k >> return False -- elementary row operation
      Nothing -> do
        -- secondary row operation
        let (alpha, beta) = gcdExpr a b
            d = alpha*a + beta*b -- d = gcd(a, b)
            a' = fromJust $ divides d a -- so d divides a and b
            b' = fromJust $ divides d b
        arbRow ((alpha,beta),(-b',a')) i k
        -- After multiplication by this matrix, m will be gcd(a, b) in position
        -- i, j, and zero in position k, j.  Since d = αa + βb, then
        -- 1 = αa/d + βb/d = αa' + βb', which is also the determinant of this
        -- matrix.
        return True

  -- Eliminate entries in the ith row, right of column i.
  -- return True if any secondary column operations were used, otherwise return
  -- False
  elimRow i = do
    m <- getM
    opTypes <- mapM (elimREntry i) . V.toList . V.map (i+1+)
               . V.findIndices (0 /=) . V.drop (i+1) $ m V.! i
    return $ foldr (||) False opTypes

  -- Eliminate the entry in row i, column k, using a column operation on
  -- columns i and k. Uses an elementary column operation if possible,
  -- otherwise uses a secondary column operation. Returns True if a secondary
  -- column operation was used.
  elimREntry i k = do
    m <- getM
    let a = m V.! i V.! i
        b = m V.! i V.! k
    case a `divides` b of
      Just q -> addCol (-q) i k >> return False
      Nothing -> do
        let (alpha, beta) = gcdExpr a b
            d = alpha*a + beta*b
            a' = fromJust $ divides d a
            b' = fromJust $ divides d b
        arbCol ((alpha,-b'),(beta,a')) i k
        return True

  -- divisorChain i assumes that the matrix is diagonal, and that the submatrix
  -- right of column i and below row i has a divisor chain in the diagonal. It
  -- forces the submatrix including row and column i to have a divisor chain,
  -- and remain diagonal.
  divisorChain i = do
    m <- getM
    -- if we're at the edge of the matrix, there's nothing to do
    when (i+1 < rows m && i+1 < columns m) $ do
      -- Assuming the 2x2 submatrix with upper-left corner at (i, i) has the
      -- form (a 0).
      --      (0 b)
      addCol 1 (i+1) i 
      -- Now it is (a 0).
      --           (b b)
      b <- elimCEntry i (i+1)
      -- Now it is (a 0) if a divides b, or (gcd(a,b)    βb   ) otherwise.
      --           (0 b)                    (   0     lcm(a,b))
      -- In the former case, we have a divisor chain, so we are done.
      when b $ do
        -- Otherwise, we can eliminate βb with an elementary column operation to
        -- obtain (gcb(a,b)    0    ).
        --        (   0     lcm(a,b))
        elimREntry i (i+1)
        -- Now the (i, i) entry divides everything below it since it divides b,
        -- but we need to fix up the remaining divisor chain.
        divisorChain (i+1)
        -- gcd(a,b) will still divide everything below it.

-- TODO: implement determinant, full row-reduction, and inverses.

test :: ((), Matrix Integer, Matrix Integer, Matrix Integer)
test = runM f . V.fromList . map V.fromList $ [[5,0],[0,6]] where
  f = smith 
    {-
    addRow (-4) 0 1
    addRow (-7) 0 2
    addRow (-2) 1 2
    addCol (-2) 0 1
    addCol (-3) 0 2
    addCol (-2) 1 2
    -}



-- calculate the Smith normal form of a matrix
-- If smith M = (A, B, C), then M = ABC, B is in Smith normal form, and A and C
-- are invertible.
--smith :: PID r => Matrix r -> (Matrix r, Matrix r, Matrix r)
--smith m = maybe findNonZero 

-- put a m x n matrix in row-reduced echelon form
-- Note: this doesn't work well for floats due to round-off error
rref :: (IntegralDomain r, Fractional r) => Matrix r -> Matrix r
rref m = rref' 0 0 m where
  r = rows m
  c = columns m
  -- rref' i j assumes that the matrix is already in rref above row i and to
  -- the left of column j
  rref' i j x | i >= r || j >= c = x
              | otherwise = maybe (rref' i (j+1) x)
                            (rref' (i+1) (j+1) . doRow . (i+))
                            . V.findIndex ((0 /=) . (V.! j)) . V.drop i $ x
    where doRow k = elimOther . normalize . swapRows' i k $ x
          normalize y = scaleRow' (1/(y V.! i V.! j)) i y
          elimOther y = V.imap f y where
            f k row | k == i = row
                    | otherwise = V.zipWith (g $ row V.! j) row $ y V.! i
            g e r1 r2 = r1 - e*r2
