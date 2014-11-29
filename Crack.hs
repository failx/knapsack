module Crack (crack) where

import Math.Lattices.LLL (lll)
import Data.Array (elems)
import Data.Ratio ((%))
import Data.List (transpose)

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

m' = transpose m

-- function for cracking a knapsack integer sequence
crack :: [Integer] -> [Integer]
crack = get_solution . apply_lll . prepare_matrix

-- filter for vector that might be a solution for knapsack problem
-- and converts into appropriate format (to integer and remove last element)
get_solution :: [[Rational]] -> [Integer]
get_solution = init . map round . head . filter is_solution

-- take a list and decide whether it is a valid bit field (containing only 1 and 0)
is_solution :: [Rational] -> Bool
is_solution rs = filter (\r -> or [r == 0, r == 1]) rs == rs

-- apply lll and return a list of list instead of list of arrays
apply_lll :: [[Rational]] -> [[Rational]]
apply_lll = elems . lll

-- take a knapsack problem and convert into attackable form
prepare_matrix :: [Integer] -> [[Rational]]
prepare_matrix is = zipWith (\i r -> i ++ [r]) id_matrix ks_as_rational
  where l = length is
        ks_as_rational= map toRational is
        id_matrix = identity_matrix (l - 1) ++ [take (l - 1) zeros]

-- build identity matrix of size n with a row of zeros appended to the right side
identity_matrix :: Int -> [[Rational]]
identity_matrix n = reverse $ id_help n n
  where id_help n 0 = []
        id_help n i = identity_vector i n : id_help n (i - 1)

-- list of zeros
zeros :: [Rational]
zeros = (0 % 1) : zeros

-- create a list of zeros of length n with a value of one at i
identity_vector :: Int -> Int -> [Rational]
identity_vector i n = map (\e -> if e == i then 1 % 1 else 0 % 1) [1 .. n]
