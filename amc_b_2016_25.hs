module Main where

import Data.List
 
-- Problem, given the following recursive series, find the first
-- index of n greater than 0 where the product
-- of a 0 * a 1 ... * a n is an integer

-- the recursive series from the problem
a 0 = 1
a 1 = 2 ** (1 / 19)
a n = a (n - 1) * a (n - 2)

-- b is the pattern in the exponents of series a
b 0 = 0
b n | n `mod` 2 == 0 = 2 * b (n - 1) - 1
    | otherwise	     = 2 * b (n - 1) + 1

-- c is the sum of the exponents in series b mod 19 (the numerator of the exponent in series a mod 19; when it is 0 there is a solution)
c n = sum (map b [0..n]) `mod` 19

-- a map of the solutions to the problem statement
solutions = elemIndices 0 $ map c[0..]

main = print $ take 10 solutions