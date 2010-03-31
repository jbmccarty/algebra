{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SymmetricAlgebra(module Algebra, SymmetricAlgebra()) where
import FreeModuleBase
import Algebra
import qualified Restricted as R
import qualified Data.Map as M

-- should include only positive powers
-- This is isomorphic to FreeModule Integer b, but I need access to the
-- internals
newtype Basis b = Basis { unBasis :: FM Integer b } deriving (Eq, Ord)

pack = Basis
unpack = unBasis
lift f = pack . f . unpack
lift2 f x y = pack $ f (unpack x) (unpack y)

showB :: Show b => FM Integer b -> String
showB = s . M.toList where
  s [] = "1"
  s [x] = showTerm x
  s (x:xs) = showTerm x ++ " " ++ s xs
  showTerm (x, 1) = show x
  showTerm (x, n) = show x ++ "^{" ++ show n ++ "}"

instance Show b => Show (Basis b) where
  show = showB . unpack

instance Ord b' => R.Functor Basis b b' where
  fmap = lift . fmapFM

instance R.MonadR Basis b where
  return = pack . returnFM

instance (Num r, Ord b) => Multiplicative r (Basis b) where
  one = [(pack zeroFM, 1)]
  mul x y = [(lift2 addFM x y, 1)]

newtype SymmetricAlgebra r b = SA { unSA :: FreeModule r (Basis b) }
  deriving (Eq, Ord, AbelianGroup, Num)

pack' = SA
unpack' = unSA
lift' f = SA . f . unSA

instance (Num r, Show b) => Show (SymmetricAlgebra r b) where
  show = show . unpack'

instance (Num r, Ord b) => Module r (SymmetricAlgebra r b) where
  r ! x = lift' (r!) $ x

instance (Num r, Ord b') => R.Functor (SymmetricAlgebra r) b b' where
  fmap = lift' . R.fmap . lift . fmapFM

instance Num r => R.MonadR (SymmetricAlgebra r) b where
  return = pack' . R.return . pack . returnFM

-- too lazy to implement bind...
