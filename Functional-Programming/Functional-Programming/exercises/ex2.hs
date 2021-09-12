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

-- Type inference

-- In these exercises you should assume the following types:

-- (+)    :: Int -> Int -> Int
-- even   :: Int -> Bool

-- head   :: [a] -> a
-- (++)   :: [a] -> [a] -> [a]
-- foldr  :: (a -> b -> b) -> b -> [a] -> b
-- map    :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- (.)    :: (b -> c) -> (a -> b) -> a -> c



-- head ([3,2] ++ [2])  :: [Int] -> [Int]

-- (+) 3                :: Int -> Int

-- map even             :: [Int] -> [Bool] 
    -- map              :: (a -> b) -> [a] -> [b]
    -- even             :: Int -> Bool

-- map concat           :: [[[a]]] -> [[a]]
    -- map              :: (a -> b) -> [a] -> [b]
    -- concat           :: [[a]] -> [a]

-- map head             :: [[a]] -> [a]
    -- map              :: (a -> b) -> [a] -> [b]
    -- head             :: [a] -> a
        -- a = [a]
        -- a = b
        -- a = [a] = b

-- reverse . reverse    :: [e] -> [e]
    -- reverse          :: [d] -> [d]
    -- reverse          :: [e] -> [e]
    -- (.)              :: (b -> c) -> (a -> b) -> a -> c
        -- b = [d] = c
        -- a = [e] = b
        -- a = b = c = [d] = [e]

-- foldr (+)            :: Int -> [Int] -> Int
    -- foldr            :: (a -> b -> b) -> b -> [a] -> b
    -- (+)              :: Int -> Int -> Int

-- foldr map            :: [c] -> [c -> c] -> [c]
    -- foldr            :: (a -> b -> b) -> b -> [a] -> b
    -- map              :: (c -> d) -> [c] -> [d]
        -- (a -> b -> b) = (c -> d) -> [c] -> [d]
        -- a = (c -> d)
        -- b = [c] = [d]
        -- c = d

-- map . foldr          :: e -> g = (c -> d -> d) -> [d] -> [[c] -> d]
    -- map              :: (a -> b) -> [a] -> [b]
    -- foldr            :: (c -> d -> d) -> d -> [c] -> d
    -- (.)              :: (f -> g) -> (e -> f) -> e -> g
        -- (f -> g) = (a -> b) -> [a] -> [b]
        -- f = (a -> b) = d -> [c] -> d
        -- a = d
        -- b = [c] -> d = [c] -> a
        -- g = [a] -> [b]
        -- (e -> f) = (c -> d -> d) -> d -> [c] -> d
        -- e = (c -> d -> d)

-- concat . concat      :: a -> c = [[[e]]] -> [e]
    -- concat           :: [[e]] -> [e]
    -- concat           :: [[f]] -> [f] 
    -- (.)              :: (b -> c) -> (a -> b) -> a -> c
        -- (b -> c) = [[e]] -> [e]
        -- (a -> b) = [[f]] -> [f]
        -- b = [[e]] = [f]
        -- c = [e]
        -- a = [[f]] = [[[e]]]

-- map map              :: [a] -> [b] = [c -> d] -> [[c] -> [d]]
    -- map              :: (a -> b) -> [a] -> [b]
    -- map              :: (c -> d) -> [c] -> [d]
        -- (a -> b) = (c -> d) -> [c] -> [d]
        -- a = c -> d
        -- b = [c] -> [d]

-- map (map map)        :: [a] -> [b] = [[e -> f]] -> [[[e] -> [f]]]
    -- map1             :: (a -> b) -> [a] -> [b]
    -- map2             :: (c -> d) -> [c] -> [d]
    -- map3             :: (e -> f) -> [e] -> [f]
        -- (c -> d) = (e -> f) -> [e] -> [f]
        -- c = e -> f
        -- d = [e] -> [f]
        -- map2 map3    :: [c] -> [d] 
        --                 = [e -> f] -> [[e] -> [f]]
        -- (a -> b) = [e -> f] -> [[e] -> [f]]
        -- a = [e -> f]
        -- b = [[e] -> [f]]

-- map map map and brackets
-- which observation is true?
    -- The type of the first is less polymorphic than the type of the second. <-
    -- The type of the first is more polymorphic than the type of the second.
    -- The types are the same, since function composition is associative.
    -- One of the expressions does not have any type at all.
        -- map (map map) can transform the types twice, whereas (map map) map can only transform the same type once???

-- map maximum          :: [a] -> [b] = Ord c => [[c]] -> [c]
    -- map              :: (a -> b) -> [a] -> [b]
    -- maximum          :: Ord c => [c] -> c
        -- (a -> b) = Ord c => [c] -> c
        -- a = Ord [c]
        -- b = Ord c
