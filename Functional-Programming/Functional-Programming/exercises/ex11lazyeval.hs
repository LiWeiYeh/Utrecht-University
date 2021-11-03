module Main where

import Data.Foldable
import Data.List

main :: IO ()
main = undefined

primes :: Int -> [Int]
primes n = take n $ sieve [2 .. ]

sieve (p:ns) = p : sieve [n | n <- ns, n `mod` p /= 0]


