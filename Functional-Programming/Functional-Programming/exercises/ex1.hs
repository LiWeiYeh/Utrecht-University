module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Main


main :: IO ()
main = putStrLn "hello world"



-- Evaluation ------------------------------------------------------------------------------------------------------

thrice x = [x, x, x]

sums (x : y : ys) = x : sums (x + y : ys)
sums xs           = xs

-- map thrice (sums [0 .. 4])
-- returns
-- map thrice ( [0, 1, 3, 6, 10] )
-- [[0,0,0], [1,1,1], [3,3,3], [6,6,6], [10,10,10]]

fac n | n == 0 = 1
      | otherwise = n * fac (n - 1)

-- fac (-3)
-- returns
-- infinite loop, there is no guard for negative numbers



-- Writing Functions -----------------------------------------------------------------------------------------------

-- Write a function noOfSol that, for some , , and , determines the number of solutions of the equation , using case distinction.
-- with guards
-- noOfSol :: Int -> Int -> Int -> Int
noOfSol a b c   | b^2 - 4*a*c > 0 = 2
                | b^2 - 4*a*c == 0 = 1
                | b^2 - 4*a*c < 0 = 0

-- with case disctinction ??????
-- noOfSol a b c = 
--     let discriminant = b^2 - 4*a*c
--         case discriminant == 0 of 
--             0 -> 1
--             discriminant > 0 -> 2
--             discriminant < 0 -> 0

-- Write a function pow2 :: Int -> Int that takes an Int computes using direct recursion.
pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

-- Write a recursive function pow that takes two Ints, and , and computes.
-- pow :: Int -> Int
-- pow x 0 = 1
-- pow x n = x * pow x (n-1)

-- For any number , and any even number it holds that . Use this to speed up the implementation of the pow function.
pow :: Int -> Int -> Int
pow x 0 = 1
pow x n | even n    = pow x (n `div` 2) * pow x (n `div` 2)
        | otherwise = x  * pow x (n-1)

-- Which intermediate results are being computed for the computation of "   2 `pow` 10   " in the old and the new definition?
-- old 
{-
    2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 1
-}
-- new
{-
    pow 2 5 * pow 2 5
    (2 * pow 2 4) * (2 * pow 2 4)
    (2 * pow 2 2 * pow 2 2) * (2 * pow 2 2 * pow 2 2)
    (2 * 2 * 2 * 2 * 1) * (2 * 2 * 2 * 2 * 1)
    2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 1
-}

-- A predicate p :: Int -> Bool is monotonic if, for any Int i for which the predicate holds (p i == True) the predicate also holds for any index j larger than i (p j is also True).
-- Given:
--     a monotonic predicate p,
--     a lowerbound l :: Int for which p l == False, and
--     an upperbound u :: Int for which p u == True,
-- implement a function binarySearch that can find the smallest Int i for which p i is True in
-- steps.
-- Also give the type of your function.

greaterThan x | x > 7 = True
              | otherwise = False

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch p l u  | u - l == 1    = u
                    | p h           = binarySearch p l h
                    | not (p h)     = binarySearch p h u
                    where
                        h = div (l+u) 2

-- Give the most general type for the function binarySearch you defined above.
-- binarySearch :: Integral i => (i -> Bool) -> i -> i -> i



-- Basic Types
-- 3                :: Int
-- even             :: Int -> Bool
-- even 3           :: Int -> Bool

-- head             :: [a] -> a
-- [1,2,3]          :: [a]
-- head [1,2,3]     :: [a] -> a

-- tail             :: [a] -> [a]
-- length           :: [a] -> Int
-- noOfSol          :: Int -> Int -> Int -> Int
-- pow2             :: Int -> Int
-- div              :: Num a => a -> a -> a
-- (/)              :: Num a => a -> a -> a
-- sqrt             :: Num a => a -> a


-- Implementing (an approximate) Square Root
-- In this (set of) exercises we will write our own implementation of the square root function. More precisely, we write a function approxSqrt that can approximate for any value
-- sqrt(x).

-- Consider the following two facts about the square root:
-- 1. if y is a good approximation of sqrt(x) then (1/2)(y + x/y) is a better approximation.
-- 2. 1 is a (not-so) good approximation of sqrt(x)

-- We will say that the approximation of sqrt(x) is good enough when y^2 is close to x. More specifically, when |y^2 - x| is at most some threshold eps.

-- Use the above two facts to implement a function approxSqrt :: Double -> Double -> Double so that approxSqrt eps x returns a value y 
-- that is a good enough (with respect to the given threshold eps).
-- approxSqrt :: Double -> Double -> Double
-- approxSqrt eps x = 
--     go 1 
--     where
--         go y =  let y' = (1/2)*(y + x/y)
--                 in  if goodEnough y 
--                     then y'
--                     else go y'
--         goodEnough y = abs (y*y - x) <= eps

-- write an alternative implementation of approxSqrt using the following function until :: (a -> Bool) -> (a -> a) -> a -> a which takes care of the actual iteration/recursion.
-- approxSqrt :: Double -> Double -> Double
-- approxSqrt eps x = until pred step 1
--     where 
--         pred y = abs (y^2-x) <= eps
--         step y = (1/2)*(y + x/y)

-- Maybe we don’t know in advance yet when the approximation is “good enough”, and instead we just want a list of ever more precise approximations of sqrt(x). 
-- Write a function approxSqrts :: Double -> [Double] that produces such a list.
approxSqrt :: Double -> [Double]
approxSqrt x =
    go 1
    where go y = y : go ((1/2) * (y + x/y))