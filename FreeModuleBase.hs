-- this is not intended to be used by user code
module FreeModuleBase where
import qualified Data.Map as M
import Data.List(foldl', intersperse)
import Module

-- A free r-module with basis b
type FM r b = M.Map b r

showsPrecFM :: (Num r, Show b) => Int -> FM r b -> ShowS
showsPrecFM p x | M.null x  = showString "0"
                | otherwise =
  showParen (p > prec) . foldr (.) id . intersperse (showString " + ") $
    [ (if r == 1 then id else showsPrec 6 r . showString " ") . showsPrec 6 b
      | (b, r) <- M.toList x ]
  where prec = 5

canonicalizeFM :: (Num r, Ord b) => FM r b -> FM r b
canonicalizeFM = M.filter (/= 0)

addFM :: (Num r, Ord b) => FM r b -> FM r b -> FM r b
addFM x y = canonicalizeFM $ M.unionWith (+) x y

-- the additive identity
zeroFM :: FM r b
zeroFM = M.empty

-- negation
negateFM :: Num r => FM r b -> FM r b
negateFM = M.map negate

-- the action of r on the module
actFM :: (Ord b, Num r) => r -> FM r b -> FM r b
actFM r = canonicalizeFM . M.map (r*)

-- FM is a (restricted) functor on the basis type
fmapFM :: (Num r, Ord b') => (b -> b') -> FM r b -> FM r b'
fmapFM f = canonicalizeFM . M.mapKeysWith (+) f

-- FM is a (restricted) monad on the basis type
returnFM :: Num r => b -> FM r b
returnFM x = M.singleton x 1

bindFM :: (Num r, Ord b') => FM r b -> (b -> FM r b') -> FM r b'
bindFM x f = foldl' addFM zeroFM [ actFM a $ f b | (b, a) <- M.toList x ]

-- FM r b is a free r-module
injectFM :: Num r => b -> FM r b
injectFM = returnFM

freeFM :: Module r m => (b -> m) -> FM r b -> m
freeFM f x = foldl' (.+.) zero [ a .* f b | (b, a) <- M.toList x ]
