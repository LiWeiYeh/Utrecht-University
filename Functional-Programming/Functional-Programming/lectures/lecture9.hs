module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

sequence_'  :: [IO'  a] -> IO' ()
sequence_'  [] = return ()
sequence_'  (ia : ias) = 
    do
        ia
        sequence_' ias

sequence' :: [IO' a] -> IO'  [a]
sequence' [] = return []
sequence' (ia : ias) = 
    do
        a <- ia
        as <- sequence' ias
        return (a:as)
sequence' (ia : ias) =
    ia >>= \a -> (
        sequence' ias >>= \as ->
            return (a:as)
    )



