module AbelianGroup where

class AbelianGroup g where
  (.+.) :: g -> g -> g
  zero :: g
  negateA :: g -> g

(.-.) :: AbelianGroup g => g -> g -> g
x .-. y = x .+. negateA y

infixl 6 .+., .-.
