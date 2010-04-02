{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TensorAlgebra(module Algebra, TensorAlgebra(), Basis(..)) where
import Algebra
import qualified Restricted as R
import Data.List(intercalate)
import Grading

newtype Basis b = Basis { unBasis :: [b] } deriving (Eq, Ord, Functor, Monad)

instance Show b => Show (Basis b) where
  show (Basis []) = "1"
  show (Basis xs) = intercalate " \\otimes " . map show $ xs

instance Num r => Multiplicative r (Basis b) where
  one = [(Basis [], 1)]
  mul (Basis xs) (Basis ys) = [(Basis $ xs ++ ys, 1)]

instance Graded b => Graded (Basis b) where
  degree = sum . map degree . unBasis
  grading = undefined -- too lazy to implement just now

newtype TensorAlgebra r b = TA { unTA :: FreeModule r (Basis b) }
  deriving (Eq, Ord, AbelianGroup, Num)

pack = TA
unpack = unTA
lift f = TA . f . unTA

instance (Num r, Show b) => Show (TensorAlgebra r b) where
  show = show . unpack

instance (Num r, Ord b) => Module r (TensorAlgebra r b) where
  r .* x = lift (r .*) $ x

instance (Num r, Ord b') => R.Functor (TensorAlgebra r) b b' where
  fmap = lift . R.fmap . fmap

instance Num r => R.MonadR (TensorAlgebra r) b where
  return = pack . R.return . return

-- too lazy to implement bind or free...
