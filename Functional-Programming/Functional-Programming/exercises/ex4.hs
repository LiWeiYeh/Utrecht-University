module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

lengthh :: [a] -> Int
lengthh xs = foldr (\_ y -> 1 + y) 0 xs

-- plusplus xs ys = 

productt :: [Int] -> Int
productt xs = foldr (*) 1 xs

orr :: [Bool] -> Bool
orr xs = foldr (||) False xs

-- any :: Bool -> 
anyy p xs = foldr (\x -> p x) False xs

-- mapp :: [a] -> [a]
-- mapp f xs = foldr (\x -> f x :) [] xs

reversee :: [a] -> [a]
reversee xs = foldl (:) [] xs