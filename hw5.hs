-- import Prelude hiding (and)
-- import Prelude hiding (concat)
-- import Prelude hiding (replicate)
-- import Prelude hiding (elem)

-- e0

-- m # 0 = 1
-- m # n = m * m # (n - 1)

-- m # 0 = 1
-- m # n = m * (#) m (n - 1)

-- e4

-- and [] = True
-- and (b : bs) = b && and bs

-- and [] = True
-- and (b : bs)
--   | b = and bs
--   | otherwise = False

-- and [] = True
-- and (b : bs)
--   | b == False = False
--   | otherwise = and bs

-- and [] = True
-- and (b : bs) = and bs && b

-- e5

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (xs : xss) = xs ++ concat xss

-- e6

-- replicate :: Int -> a -> [a]
-- replicate 0 _ = []
-- replicate n x = x : replicate (n - 1) x

-- e7

-- (#) :: [a] -> Int -> a
-- (x : _) # 0 = x
-- (_ : xs) # n = xs # (n - 1)

-- e8

-- elem :: Eq a => a -> [a] -> Bool
-- elem _ [] = False
-- elem x (y : ys)
--   | x == y = True
--   | otherwise = elem x ys

-- e9

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

-- e10

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs
