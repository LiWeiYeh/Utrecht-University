module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

anyy p xs = foldr f e xs
                where
                    f x r = p x || r
                    e = False

productt xs = foldr f e xs
                where 
                    f x r = x * r
                    e = 1

alll p xs = foldr f e xs
                where 
                    f x r = p x && r
                    e = True

mapp g xs = foldr f e xs
                where
                    f x r = g x: r
                    e = []

reversee xs = foldr f e xs
                where
                    f x r = r ++ [x]
                    e = []

concatt xs = foldr f e xs
                where
                    f x r = x ++ r
                    e = []

concatMapp g xs = foldr f e xs
                where
                    f x r = g x ++ r
                    e = []

elemm :: Eq a => a -> [a] -> Bool
elemm y xs = foldr f e xs
                where
                    f x r = x == y || r
                    e = False

filterr p xs = foldr f e xs
                where
                    f x r | p x = x : r
                          | otherwise = r
                    e = []

maybeLastt xs = foldr f e xs
                where
                    f x r = case r of 
                            Nothing -> Just x
                            Just _ -> r
                    e = Nothing

partitionn p xs = foldr f e xs
                    where
                        f x (ts,fs) | p x = (x:ts,fs)
                                    | otherwise = (ts, x:fs)
                        e = ([],[])

unzipp :: [(a,b)] -> ([a],[b])
unzipp xs = foldr f e xs
                where
                    f (x1,x2) (ts, fs) = (x1:ts, x2:fs)
                    e = ([],[])

unliness xs = foldr f e xs
                where
                    f x r = x ++ "\n" ++ r
                    e = ""

nubb xs = foldr f e xs
            where
                f x r | x `elem` r = r
                      | otherwise = x : r
                e = []

unionn :: Eq a => [a] -> [a] -> [a]
unionn xs ys = foldr f e ys
                where
                    f x r | x `elem` xs = r
                          | otherwise = x : r
                    e = xs

intersectt :: Eq a => [a] -> [a] -> [a]
intersectt xs ys = foldr f e xs
                    where
                        f x r | x `elem` ys = x : r
                              | otherwise = r
                        e = []

-- sortt :: Eq a => [a] -> [a]
sortt xs = foldr f e xs
                where
                    f x r = insert x r
                    e = []

-- unzipp :: [(a,b)] -> ([a], [b])
unzippp [] = ([],[])
unzippp ((x1,x2):xs) = let (ts, fs) = unzipp xs in (x1:ts, x2:fs)

nulll :: [a] -> Bool
nulll xs = foldr f e xs
            where
                f x r = False
                e = True

interspersee :: a -> [a] -> [a]
interspersee value xs = foldr f e xs
                            where
                                f x r = case r of 
                                        []  -> [x]
                                        _   -> x : value : r
                                e     = []

permutationss :: [a] -> [[a]]
permutationss xs = foldr f e xs  
                    where
                        f x r = concatMapp (insertEverywhere x) r
                        e = [[]]
insertEverywhere             :: a -> [a] -> [[a]]
insertEverywhere x []        = [[x]]
insertEverywhere x xs@(y:ys) = (x:xs) : map (y:) (insertEverywhere x ys)

takeWhilee p xs = foldr f e xs
                    where
                        f x r | p x = x : r
                              | otherwise = []
                        e = []








-- THE LECTURE ----->>>>>

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








