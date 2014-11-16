module caesar where

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
          6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent n m = (fromInt n / fromInt m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqroses = sum [((o − e) ^ 2) / e | (o, e) <- ziposes]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String → String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitable) chitable)
    chitable = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
