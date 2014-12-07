module GameOfLife where

type Pos = (Int, Int)
type Board = [Pos]

-- Auxiliary functions

goto :: Pos -> IO ()
goto (x, y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a : as) = do _ <- a
                   seqn as

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

cls :: IO ()
cls = putStr "\ESC[2J"

-- GameOfLife

width :: Int
width = 5

height :: Int
height = 5

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                           (x + 1, y - 1), (x - 1, y),
                           (x + 1, y), (x - 1, y + 1),
                           (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concatMap neighbs b),
                     isEmpty b p,
                     liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 5000
            life (nextgen b)
