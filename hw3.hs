-- e1

-- safetail xs = if null xs then [] else tail xs

-- safetail [] = []
-- safetail (_ : xs) = xs

-- safetail xs
--   | null xs = []
--   | otherwise = tail xs

-- safetail [] = []
-- safetail xs = tail xs

safetail
  = \ xs ->
    case xs of
      [] -> []
      (_ : xs) -> xs

-- e2

-- import Prelude hiding ((||))

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

-- b || c
--   | b == c = b
--   | otherwise = True

-- b || False = b
-- _ || True = True

-- b || c
--   | b == c = c
--   | otherwise = True

-- b || True = b
-- _ || True = True

False || False = False
False || True = True
True || False = True
True || True = True

-- e3

-- import Prelude hiding ((&&))

-- True && True = True
-- _ && _ = False

-- a && b = if a then if b then True else False else False
-- a && b = if a then b else False
-- a && b = if b then a else False

-- a && b = if not (a) then not (b) else True
-- a && b = if a then b
-- a && b = if a then if b then False else True else False


-- e4

-- mult x y z = \ x -> (\ y -> (\ z -> x * y * z))
-- mult = \ x -> (\ y -> (\ z -> x * y * x))
-- mult = \ x -> (x * \ y -> (y * \ z -> z))
-- mult = ((((\x -> \y) -> \z) -> x * y) * z)

-- e8

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs
