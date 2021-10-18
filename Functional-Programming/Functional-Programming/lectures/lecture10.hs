module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

mapMay :: (a -> b) -> Maybe a -> Maybe b
mapMay Nothing = Nothing
mapMay f (Just a) = Just (f a)

data Tree a = Leaf
             | Node (Tree a) a (Tree a)
             deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r) 

instance Functor IO where
    fmap f ia = do
        a <- ia
        return (f a)

-- instance Functor [] where
notFMap :: (a -> b) -> [a] -> [b]
notFMap f [] = []
notFMap f (x:xs) = []
