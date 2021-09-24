module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

lengthh :: [a] -> Int
lengthh = foldr (\_ y -> 1 + y) 0

-- plusplus xs ys = 

productt :: [Int] -> Int
productt = foldr (*) 1

orr :: [Bool] -> Bool
orr = foldr (||) False

anyy p = foldr f e
                where
                    f x r = p x || r
                    e = False

productt = foldr f e
                where 
                    f x r = x * r
                    e = 1

alll p = foldr f e
                where 
                    f x r = p x && r
                    e = True

mapp g = foldr f e
                where
                    f x r = g x: r
                    e = []

reversee = foldr f e
                where
                    f x r = r ++ [x]
                    e = []

concatt = foldr f e
                where
                    f x r = x ++ r
                    e = []

concatMapp g = foldr f e
                where
                    f x r = g x ++ r
                    e = []

elemm :: Eq a => a -> [a] -> Bool
elemm y = foldr f e
                where
                    f x r = x == y || r
                    e = False

filterr p = foldr f e
                where
                    f x r | p x = x : r
                          | otherwise = r
                    e = []

maybeLastt = foldr f e
                where
                    f x r = case r of 
                            Nothing -> Just x
                            Just _ -> r
                    e = Nothing

partitionn p = foldr f e
                where
                    f x (ts,fs) | p x = (x:ts,fs)
                                | otherwise = (ts, x:fs)
                    e = ([],[])

unzipp :: [(a,b)] -> ([a],[b])
unzipp = foldr f e
            where
                f (x1,x2) (ts, fs) = (x1:ts, x2:fs)
                e = ([],[])

unliness = foldr f e
            where
                f x r = x ++ "\n" ++ r
                e = ""

nubb = foldr f e
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
sortt = foldr f e
            where
                f x r = insert x r
                e = []

-- unzipp :: [(a,b)] -> ([a], [b])
unzippp [] = ([],[])
unzippp ((x1,x2):xs) = let (ts, fs) = unzipp xs in (x1:ts, x2:fs)

nulll :: [a] -> Bool
nulll = foldr f e
            where
                f x r = False
                e = True

interspersee :: a -> [a] -> [a]
interspersee value = foldr f e
                        where
                            f x r = case r of 
                                    []  -> [x]
                                    _   -> x : value : r
                            e     = []

permutationss :: [a] -> [[a]]
permutationss = foldr f e  
                    where
                        f x r = concatMapp (insertEverywhere x) r
                        e = [[]]
insertEverywhere             :: a -> [a] -> [[a]]
insertEverywhere x []        = [[x]]
insertEverywhere x xs@(y:ys) = (x:xs) : map (y:) (insertEverywhere x ys)

takeWhilee p = foldr f e
                where
                    f x r | p x = x : r
                            | otherwise = []
                    e = []