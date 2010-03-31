{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KZ2(KZ2(), iota) where
import Steenrod
import Algebra
import TensorAlgebra
import Z2
import qualified Restricted as R
import Data.TypeLevel(Nat, toNum)
-- from the type-level package in hackage
import Control.Arrow(first)

newtype KZ2B n = KZ2B { unKZ2B :: Steenrod.Basis } deriving (Eq, Ord)

-- NatI n
-- use toNum
instance Nat n => Show (KZ2B n) where
  show (KZ2B xs :: KZ2B n) = show xs ++ " \\iota_{"
                             ++ show (toNum (undefined :: n)) ++ "}"

type KZ2 n = TensorAlgebra Z2 (KZ2B n)

iota :: Nat n => n -> KZ2 n
iota _ = 1 R.>>= (returnTA . KZ2B)

instance Nat n => AModule (KZ2B n) where
  sq' r (KZ2B x :: KZ2B n) = map KZ2B . filter ((< m) . excess) . sq' r
                             $ x
    where m = toNum (undefined :: n)
