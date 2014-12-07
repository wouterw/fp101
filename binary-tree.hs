module BinaryTree where

data Tree = Leaf Int | Node Tree Int Tree

sampleTree :: Tree
sampleTree = Node (Node (Leaf 1) 3 (Leaf 4))
                  5
                  (Node (Leaf 6) 7 (Leaf 9))

occurs               :: Int -> Tree -> Bool
occurs m (Leaf n)     = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r

flatten             :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- A tree is a search tree if it flattens to a list that is ordered.

occurs' :: Int -> Tree -> Bool
occurs' m (Leaf n)              = m == n
occurs' m (Node l n r) | m == n = True
                       | m < n  = occurs m l
                       | m > n  = occurs m r

