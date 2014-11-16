-- import Prelude hiding (all)
-- import Prelude hiding (any)
-- import Prelude hiding (takeWhile)
-- import Prelude hiding (dropWhile)
-- import Prelude hiding (map)

-- e1

-- all :: (a -> Bool) -> [a] -> Bool
-- all p xs = and (map p xs)
-- all p = and . map p
-- all p = not . any (not . p)
-- all p xs = foldl (&&) True (map p xs)
-- all p = foldr (&&) True . map p

-- e2

-- any :: (a -> Bool) -> [a] -> Bool
-- any p = or . map p
-- any p xs = length (filter p xs) > 0
-- any p = not . null . dropWhile (not . p)
-- any p xs = not (all (\x -> not (p x)) xs)
-- any p xs = foldr (\x acc -> (p x) || acc) False xs
-- any p xs = foldr (||) True (map p xs)

-- e3

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--   | p x = x : takeWhile p xs
--   | otherwise = []

-- e4

-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- dropWhile _ [] = []
-- dropWhile p (x : xs)
--   | p x = dropWhile p xs
--   | otherwise = x : xs

-- e5

-- map :: (a -> b) -> [a] -> [b]
-- map f = foldl (\ xs x -> xs ++ [f x]) []

-- e6

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p = foldr (\ x xs -> if p x then x : xs else xs) []

-- e7

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

-- e8

-- compose :: [a -> a] -> (a -> a)
-- compose = foldr (.) id
-- sumsqreven = compose [sum, map (^ 2), filter even]

-- e9

-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry f = \ x y -> f (x, y)

-- e10

-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry f = \ (x, y) -> f x y

-- e11

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)

-- e12

-- map :: (a -> b) -> [a] -> [b]
-- map f = unfold null (f . head) tail

-- map (+1) [1,2,3]

-- e13

-- iterate :: (a -> a) -> a -> [a]
-- iterate f = unfold (const False) id f
