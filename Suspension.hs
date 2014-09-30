{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Suspension(Suspend, suspend) where
import Grading
import Natural
import FreeModule
import qualified Data.Set as S
import Steenrod
import qualified Restricted as R
import Basis

newtype Basis (n :: Nat) b = Basis { unBasis :: b } deriving (Eq, Ord)

pack = Basis
unpack = unBasis

instance R.MonadR (Basis n) b where
  return = pack

instance (KnownNat n, Show b) => Show (Basis n b) where
  showsPrec p (b :: Basis n b) = showParen (p > 7) $ showString "\\sigma^{"
    . showsPrec 0 n' . showString "} " . showsPrec p (unpack b)
    where n' = natVal (Proxy :: Proxy n)
  -- showsPrec p = showsPrec p . unpack

instance (Ord b, Graded b, KnownNat n) => Graded (Basis n b) where
  degree (Basis b :: Basis n b) = n' + degree b
    where n' = natVal (Proxy :: Proxy n)
  grading = S.map pack . grading . (subtract n') :: Integer -> S.Set (Basis n b)
    where n' = natVal (Proxy :: Proxy n)

type SuspendBasis n r b = FreeModule r (Basis n b)
type Suspend n m = SuspendBasis n (URing m) (UBasis m)

instance (Ord b, AModule b) => AModule (Basis n b) where
  sq' m = include . sq' m . unpack

suspend :: (Eq r, Num r, Ord b) => FreeModule r b -> SuspendBasis n r b
suspend = include
