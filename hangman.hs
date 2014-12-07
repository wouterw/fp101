module Hangman where

import System.IO

getCh :: IO Char
getCh =
  do hSetEcho stdin False
     c <- getChar
     hSetEcho stdin True
     return c

diff :: String -> String -> String
diff xs ys =
  [if x `elem` ys then x else '-' | x <- xs]

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x:xs) = do putChar x
--                    putStr xs

-- putStrLn :: String -> IO ()
-- putStrLn xs = do putStr xs
--                  putChar '\n'

-- getLine :: IO String
-- getLine = do x <- getChar
--   if x == '\n' then
--     return []
--   else
--     do xs <- getLine
--       return (x:xs)

sgetLine :: IO String
sgetLine =
  do x <- getCh
     if x == '\n' then
       do putChar x
          return []
     else
       do putChar '_'
          xs <- sgetLine
          return (x:xs)

guess :: String -> IO ()
guess word =
  do putStr "> "
     xs <- getLine
     if xs == word then
       putStrLn "You got it!"
     else
       do putStrLn (diff word xs)
          guess word

hangman :: IO ()
hangman =
  do putStrLn "Think of a word: "
     word <- sgetLine
     putStrLn "Try to guess it:"
     guess word
