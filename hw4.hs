import Data.Char (ord, chr)
import Data.Ix (inRange)

-- e1

rep n a = [a | _ <- [1..n]]

-- e2

pyths n
  = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
    x ^ 2 + y ^ 2 == z ^ 2]

-- e3

factors n =
  [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- e4

-- [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

-- concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- e5

find k t = [v | (k', v) <- t, k == k']
-- find 1 (zip [1,2,3] [0..3])

positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

-- e6

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

-- e7

let2int :: Char -> Char -> Int
let2int base char = ord char - ord base

int2let :: Char -> Int -> Char
int2let base int = chr (ord base + int)

shift :: Int -> Char -> Char
shift n c
  | inRange ('a', 'z') c = int2let 'a' (((let2int 'a' c) + n) `mod` 26)
  | inRange ('A', 'Z') c = int2let 'A' (((let2int 'A' c) + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- e12

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

-- e13

divides x y =  x `mod` y == 0
divisors x = [d | d <- [1..x], x `divides` d]
