{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Z2(Z2()) where

data Z2 = O | I deriving (Eq, Ord)

instance Show Z2 where
  show O = "0"
  show I = "1"

znot :: Z2 -> Z2
znot O = I
znot I = O

instance Num Z2 where
  (+) O = id
  (+) I = znot
  (*) O = const O
  (*) I = id
  fromInteger n | odd n     = I
                | otherwise = O
  negate = id
  abs = undefined
  signum = undefined
