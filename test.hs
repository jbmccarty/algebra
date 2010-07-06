import FreeModule
import Z2
import Steenrod
import Natural
import Grading
import qualified Data.Set as S
import DyerLashof
import Suspension
import Sequence
import Data.List(genericReplicate)

-- If X' represents the cohomology of a spectrum X, and x::X', then sseqEntry x
-- i j is a basis for E_1^{i,j}(\Omega^\infty X). The dummy argument x is not
-- used, but accomplishes typeclass selection.
sseqEntry :: (Ord b, Graded b) => FreeModule Z2 b -> Integer -> Integer
  -> [UBasis (FreeDyerLashof b)]
sseqEntry _ i j = S.toList . bigrading $ (-i, i+j)

-- If X' represents the cohomology of a spectrum X, and x::X', then
-- sseqTable x l r b t makes a LaTeX document containing a table with columns
-- l..r, rows b..t, and whose (i,j) entry is a basis for
-- E_1^{i,j}(\Omega^\infty X). The dummy argument x is not used, but
-- accomplishes typeclass selection.
sseqTable :: (Ord b, Graded b, Show b) => FreeModule Z2 b -> Integer -> Integer
  -> Integer -> Integer -> String
sseqTable x l r b t = header ++ rows ++ footer where
  header = 
       "\\documentclass[10pt]{article}\n"
    ++ "\\begin{document}\n"
    ++ "\\begin{tabular}{l|" ++ (concat $ genericReplicate (r-l+1) "l|") ++ "}\n"
    ++ concatMap ((" & " ++) . show) [l..r] ++ "\\\\\\hline\n"
  rows = concatMap row $ reverse [b..t]
  row i = show i ++ concatMap ((" & " ++) . entry i) [l..r]
    ++ "\\\\\\hline\n"
  entry i j = entry_header ++ entries ++ entry_footer where
    entry_header = "$\\begin{array}{@{}l@{}}\n"
    entries = concatMap ((++ " \\\\\n") . show) $ sseqEntry x j i
    -- note that sseqEntry expects (column, row), while (i, j) is (row, column)
    entry_footer = "\\end{array}$\n"
  footer = 
       "\\end{tabular}\n"
    ++ "\\end{document}\n"

stuff :: String -> String
stuff = concatMap (s . words) . lines where
  dummy :: Nat n => n -> Suspend n Steenrod
  dummy _ = undefined
  s ["table", n, l, r, b, t] = useNatural f (read n) where
    f m = sseqTable (dummy m) (read l) (read r) (read b) (read t)
  s ["d", n, r, column, row] = useNatural f (read n) where
    f m = concat [ show x ++ " -> " ++ show (differential (read r) x) ++ "\n"
                   | x <- map inject $
                   sseqEntry (dummy m) (read column) (read row) ]
  s _ =
       "commands:\n"
    ++ "  table n l r b t: output a LaTeX document containing a table of the\n"
    ++ "  basis elements of E_1^{p, q}(\\Omega^\\infty \\Sigma^n HZ/2) for\n"
    ++ "  l <= p <= r and b <= q <= t\n\n"
    ++ "  d n r p q: compute d_{2^r}(E_1^{p,q}(\\Omega^\\infty \\Sigma^n HZ/2)\n"

main = interact stuff
