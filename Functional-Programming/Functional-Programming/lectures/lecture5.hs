module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

data Point = Pt Float Float
data Shape = Rectangle Point Float Float
           | Circle Point Float
           | Triangle Point Point Point

perimeter :: Shape -> Float
perimeter (Rectangle p w h) = 2 * (w + h)
perimeter (Circle p r) = 2 * pi * r
perimeter (Triangle a b c) = dist a b + dist b c + dist c a where
    dist (Pt x1 y1) (Pt x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

data IntList
    = EmptyList | Cons Int IntList

data IntTree
    = EmptyTree | Node Int IntTree IntTree

exampleTree :: IntTree
exampleTree = Node 1 (Node 2 (Node 3 EmptyTree EmptyTree) EmptyTree) (Node 4 EmptyTree EmptyTree)

elemInTree :: Int -> IntTree -> Bool
elemInTree i EmptyTree = False
elemInTree i (Node v l r) = i == v || elemInTree i l || elemInTree i r

treeHeight :: IntTree -> Int
treeHeight EmptyTree = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

treeSize :: IntTree -> Int
treeSize EmptyTree = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

-- treeToList :: IntTree -> IntList
-- treeToList EmptyTree = EmptyList
-- treeToList (Node v l r) = Cons v (ListConcat (treeToList l) ListConcat(treeToList r))

findd :: (a -> Bool) -> [a] -> Maybe a
findd p [] = Nothing
findd p (x:xs) | p x = Just x
              | otherwise = findd p xs


data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)

