{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Suspension(Suspend) where
import Grading
import Natural
import FreeModule
import qualified Data.Set as S
import Steenrod
import qualified Restricted as R
import Basis

newtype Basis n b = Basis { unBasis :: b } deriving (Eq, Ord)

pack = Basis
unpack = unBasis

instance R.MonadR (Basis n) b where
  return = pack

instance (Nat n, Show b) => Show (Basis n b) where
  showsPrec p (b :: Basis n b) = showParen (p > 5) $ showString "\\sigma^{"
    . showsPrec 0 n' . showString "} " . showsPrec p (unpack b)
    where n' = toNum (undefined :: n)
  -- showsPrec p = showsPrec p . unpack

instance (Ord b, Graded b, Nat n) => Graded (Basis n b) where
  degree (Basis b :: Basis n b) = n' + degree b
    where n' = toNum (undefined :: n)
  grading = S.map pack . grading . (subtract n') :: Integer -> S.Set (Basis n b)
    where n' = toNum (undefined :: n)

type Suspend n m = FreeModule (URing m) (Basis n (UBasis m))

instance (Ord b, AModule b) => AModule (Basis n b) where
  sq' m = include . sq' m . unpack
