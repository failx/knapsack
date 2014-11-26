module Crack where

import Math.Lattices.LLL (lll, lllDelta)
import Data.Array (elems)
import Data.Ratio ((%))

m = [[1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
     [0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
     [0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
     [0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
     [0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
     [0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
     [0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
     [0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
     [0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
     [575, 436, 1586, 1030, 1921, 569, 721, 1183, 1570, -6665]] :: [[Rational]]

-- take a list and decide whether it is a valid bit field
is_solution :: [Rational] -> Bool
is_solution rs = filter (\r -> or [r == 0, r == 1]) rs == rs

crack :: [[Rational]] -> [[Rational]]
crack = filter is_solution . elems . lll 

crack' m = filter is_solution . elems $ lllDelta m (9 % 10)
