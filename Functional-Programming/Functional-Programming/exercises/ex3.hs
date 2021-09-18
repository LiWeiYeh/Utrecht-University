module Main where

import Lib

import Data.Foldable
main :: IO ()
main = putStrLn "hello world"

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

concatt :: [[a]] -> [a]
concatt [] = []
concatt (xs:xxs) = xs ++ concatt (xxs)

andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) = x && and xs

orr :: [Bool] -> Bool
orr [] = True
orr (x:xs) | x == True = True
           | otherwise = orr xs

alll :: (a -> Bool) -> [a] -> Bool
alll _ [] = True
alll p (x:xs) | p x = alll p (xs)
              | otherwise = False

mapp :: (a -> b) -> [a] -> [b]
mapp _ [] = []
mapp f (x:xs) = f x : mapp f xs

intersperse :: Char -> [Char] -> [Char]
intersperse sep [] = []
intersperse sep [a] = [a]
intersperse sep (x:xs) = x : sep : intersperse sep xs

concatMapp :: (a -> [b]) -> [a] -> [b]
concatMapp f xs = concatt (mapp f xs)
                    where
                        concatt [] = []
                        concatt (x:xs) = x ++ concatt xs

unliness :: [String] -> String
unliness [] = []
-- unliness [a] = a
unliness (x:xs) = x ++ "\n" ++ unliness xs

filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr p (x:xs) | p x = x : filterr p xs
                 | otherwise = filterr p xs

partitionn _ [] = [[],[]] 
partitionn p xs = [filter p xs, filter (not . p) xs]

-- unzipp :: [(a,b)] -> ([a], [b])
-- unzipp [] = ([],[])
-- unzipp (x:xs) = 

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x = n : (x:xs)
                | otherwise = x : insert n xs

sortt :: Ord a => [a] -> [a]
sortt [] = []
sortt [a] = [a]
sortt (x:xs) =  insert x (sortt xs)

takee :: Int -> [a] -> [a]
takee _ [] = []
takee n (x:xs) | length (x:xs) < n  = x:xs
               | n == 0             = []
               | otherwise          = x : takee (n-1) (xs)

-- ????
-- takeWhilee :: (a -> Bool) -> [a] -> [a]
-- takeWhilee _ [] = []
-- takeWhilee p (x:xs) = mapp filterr p 

-- groupp :: [Char] -> [[Char]]
-- group [] = []
-- groupp (x:y:ys) | x == y = 
--                 |

remSuccessiveduplicatess :: [Int] -> [Int]

remSuccessiveduplicatess [] = []
remSuccessiveduplicatess [a] = [a]
remSuccessiveduplicatess (x:y:ys) | x /= y = x : remSuccessiveduplicatess (y:ys)
                                  | otherwise = remSuccessiveduplicatess (y:ys)

