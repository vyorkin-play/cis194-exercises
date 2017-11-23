module Fibonacci where

import Data.Foldable (foldl)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst . iterate (\(a, b) -> (a + b, a)) $ (0, 1)
