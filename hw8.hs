module Exercises where

import Control.Monad

-- e1

putStr'          :: String -> IO ()
putStr' []       = return ()
putStr' (x : xs) = putChar x >> putStr' xs

-- putStr' (x : xs) = do putChar x
--                       putStr' xs

-- e2

putStrLn'    :: String -> IO ()

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

putStrLn'' [] = putChar '\n'
putStrLn'' xs = putStr' xs >> putChar '\n'

putStrLn''' [] = putChar '\n'
putStrLn''' xs = putStr' xs >>= \ x -> putChar '\n'

putStrLn'''' [] = putChar '\n'
putStrLn'''' xs = putStr' xs >> putStrLn "\n"

-- e3

getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
         '\n' -> return xs
         _ -> get (xs ++ [x])

-- e4

interact' :: (String -> String) -> IO ()
interact' f
  = do input <- getLine'
       putStrLn' (f input)

-- e5

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = m >> sequence_' ms

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = m >>= \_ -> sequence_' ms

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return()) ms

-- e6

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms)
  = m >>=
      \ a ->
        do as <- sequence' ms
           return (a : as)

sequence' :: Monad m => [m a] -> m [a]
sequence' ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc
      = do x <- m
           xs <- acc
           return (x : xs)

sequence' :: Monad m => [m a] -> m [a]
sequence' (m : ms)
= do a <- m
     as <- sequence' ms
     return (a : as)

-- e7

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a : as)
  = f a >>= \b -> mapM' f as >>= \bs -> return (b : bs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a : as)
  = do b <- f a
       bs <- mapM' f as
       return (b : bs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a : as)
  = f a >>=
       \ b ->
         do bs <- mapM' f as
            return (bs ++ [b])

-- e8

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ []     = return []
filterM' p (x:xs) = do
  flg <- p x
  ys  <- filterM' p xs
  return (if flg then (x : ys) else ys)

-- e9

foldLeft :: (b -> a -> b) -> b -> [a] -> b
foldLeft f b [] = b
foldLeft f b (a:as) = foldLeft f (f b a) as

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (b : bs) = f a b >>= \a' -> foldLeftM f a' bs

-- foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

-- e10

foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f b [] = b
foldRight f b (a : as) = f a $ foldRight f b as

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (a : as) = (foldRightM f b as) >>= \b' -> f a b'

-- foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1, 3..10]) >>= \r -> putStrLn r

-- e11

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m
  = do x <- m
       return (f x)

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = m >>= \a -> return (f a)
