module Main where

import Lib

main :: IO ()
main = putStrLn "hello world"



-- primes :: Int -> [Int]
-- primes n =  [ x
--                 | x <- [2 .. n], isPrime x
--             ]
--             where
--                 isPrime x = length (factors x) == 2
--                 factors x = [ f
--                                 | f <- [1 .. x], x `mod` f == 0
--                             ]



-- quicksort :: [Int] -> [Int]
-- quicksort [] = []
-- quicksort (pivot : rest) = 
--     quicksort smaller ++ [pivot] ++ quicksort larger
--         where smaller = [x | x <- rest, x <= pivot]
--               larger =  [x | x <- rest, x > pivot]



-- sum :: [Int] -> Int
-- sum [] = 0
-- sum (x:xs) = x + sum xs



-- elem :: Eq a => a -> [a] -> Bool
-- elem x [] = False
-- -- elem x (y:ys) | x == y = True
-- --               | otherwise = elem x ys
-- elem x (y:ys) = x == y || elem x ys



-- take :: Int -> [a] -> [a]
-- take 0 _ = []
-- take n [] = []
-- take n (x:xs) = x : take (n-1) xs



-- init :: [a] -> [a]
-- init [] = Error "No elements"
-- init [_] = []
-- init (x:xs) = x : init xs



-- merge :: Ord a => [a] -> [a] -> [a]
-- merge [] [] = []
-- merge [] (y:ys) = y:ys
-- merge (x:xs) [] = x:xs
-- merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
--                     | otherwise = y : merge (x:xs) ys



-- reverse :: [a] -> [a]
-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]



-- sum :: [Int] -> Int
-- sum xs = sum' 0 xs
--     where
--         sum' :: Int -> [Int] -> Int
--         sum' acc [] = acc
--         sum' acc (x:xs) = sum' (acc+x) xs



-- reverse :: [a] -> [a]
-- reverse xs = reverse' [] xs
--     where
--         reverse' :: [a] -> [a] -> [a]
--         reverse' acc [] = acc
--         reverse' acc (x:xs) = reverse' (x : acc) xs
