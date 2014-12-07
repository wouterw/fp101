module Fibonacci where

-- https://www.haskell.org/haskellwiki/The_Fibonacci_sequence

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fib :: Integer -> Integer
-- fib n = fibs !! n
