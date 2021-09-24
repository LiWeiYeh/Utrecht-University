module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

-- type Successor a = [a]

-- data Tree a = Leaf 
--             | Node (Tree a) a (Tree a) 
--             deriving (Show, Eq)

-- elems :: Tree a -> [a]
-- elems Leaf = []
-- elems (Node l x r) = elems l ++ [x] ++ elems r

-- isBST :: Tree a -> Bool
-- isBST Leaf = True
-- isBST (Node l x r) = all (< x) (elems l) && 
--                      all (> x) (elems r) &&
--                      isBST l && isBST r

-- isSucOf :: Ord a => a -> Tree a -> Maybe a
-- isSucOf q Leaf = Nothing
-- isSucOf q (Node l x r) | q > x = isSucOf q r
--                        | otherwise = 
--                            case isSucOf q l of
--                                Just y -> Just y
--                                Nothing -> Just x

-- insert :: Ord a => a -> Tree a -> Tree a
-- insert x Leaf = Node Leaf x Leaf
-- insert x (Node l y r) | x < y  = Node ((insert x l) y r)
--                       | x == y = Node (l y r)
--                       | otherwise = Node (x y (insert x r))