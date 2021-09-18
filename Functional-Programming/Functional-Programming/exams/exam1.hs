module Main where

import Lib

main :: IO ()
main = putStrLn "hello world"

multipleFS n | n == 5 = True
             | n == 7 = True
             | otherwise = False

em2 xs = [x | x <- xs, not (multipleFS x)]

-- em3 xs = filter (not . multipleFS) xs
em3 xs = filter (\x -> not (multipleFS x)) xs






