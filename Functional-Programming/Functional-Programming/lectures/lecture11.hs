module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

-- foldl op e [x]
-- =
-- foldl op e (x:[])
-- =
-- foldl op (op e x) []
-- = 
-- op e x 
-- =
-- x

-- map f :: [a] -> [b]
-- (x :) :: [a] -> [a]

-- (map f . (x :)) xs
-- =
-- map (f (x:)) xs


-- =
-- ((f x :) . map f) xs
