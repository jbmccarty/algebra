module Util (prod, cartesian, partitions, partitionsL, partitionsND,
             partitionsNDL, choose) where
import Control.Monad(liftM2)

-- single cartesian product
prod :: [a] -> [b] -> [(a, b)]
prod = liftM2 (,)

-- list cartesian product:
-- cartesian [a1, ..., an] = [ [x1, ..., xn] | x1 <- a1, ..., xn <- an ]
cartesian :: [[a]] -> [[a]]
cartesian = foldr (liftM2 (:)) [[]]

-- partitionsL m n k is the set of all partitions of n having length k, whose
-- elements are at least m.
-- E.g. partitionsL 1 5 3 = [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]]
partitionsL :: Integer -> Integer -> Integer -> [[Integer]]
partitionsL m n k | n < 1 || k < 1 || m < 1 = error "undefined"
partitionsL m n k | k > n || m > n = []
partitionsL _ n 1 = [[n]]
partitionsL m n k = [ j:p | j <- [m..n-k+1], p <- partitionsL m (n-j) (k-1) ]

-- partitionsNDL m n k is the set of nondecreasing partitions of n having
-- length k, whose elements are at least m.
-- E.g. partitionsNDL 1 5 3 = [[1,1,3],[1,2,2]]
partitionsNDL :: Integer -> Integer -> Integer -> [[Integer]]
partitionsNDL m n k | n < 1 || k < 1 || m < 1 = error "undefined"
partitionsNDL m n k | k > n || m > n = []
partitionsNDL _ n 1 = [[n]]
partitionsNDL m n k = [ j:p | j <- [m..n-k+1], p <- partitionsNDL m (n-j) (k-1),
                        head p >= j ]

-- partitions m k is the set of all partitions of k, whose elements are at
-- least m.
-- E.g. partitions 1 5 = [[5],[1,4],[2,3],[3,2],[4,1],[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1],[1,1,1,2],[1,1,2,1],[1,2,1,1],[2,1,1,1],[1,1,1,1,1]]
partitions :: Integer -> Integer -> [[Integer]]
partitions m n = concat [ partitionsL m n k | k <- [1..n `div` m] ]

-- partitionsND m k is the set of nondecreasing partitions of k, whose elements
-- are at least m.
-- E.g. partitionsND 1 5 = [[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]
partitionsND :: Integer -> Integer -> [[Integer]]
partitionsND m n = concat [ partitionsNDL m n k | k <- [1..n `div` m] ]

-- choose n r is the coefficient of x^r in the expansion of (1 + x)^n.
-- If n is negative, this is the coefficient in the Taylor series.
choose :: Integer -> Integer -> Integer
choose n r | r < 0 || (n >= 0 && r > n) = 0
choose n r = product [n-r+1..n] `div` product [1..r]
