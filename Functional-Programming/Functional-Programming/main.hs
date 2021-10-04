module Main where

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

halve xs = let 
                ls = take ((length xs) `div` 2) xs
                rs = drop ((length xs) `div` 2) xs
            in
                (ls, rs)


-- Exam notes:

-- data Pair = MkPair a b

-- instance Eq b => Eq (Pair a b) where
--     MkPair _ b == MkPair _ b' = b == b'

-- instance Ord b => Ord (Pair a b) where
--   z <= z' = getB z <= getB z'


data WTree a = Leaf Int
             | Node (WTree a) a (WTree a)

exampleWTree = Node (
                    (Leaf 3)
                    10
                    Node (
                        (Leaf 36)
                        40
                        (Leaf 120)
                    ))


any p = foldr f x0 
    where
        f x r = p x || r
        x0 = False

-- partitionn :: (a -> Bool) -> [a] -> ([a],[a])
-- partitionn p (x:xs) | p x = let (ts,fs) = partitionn p xs in (x:ts, fs)
--                     | otherwise = let (ts,fs) = partitionn p xs in (ts, x:fs)

partitionn p = foldr f e
                    where
                        f x (ts,fs) | p x = (x:ts, fs)
                                    | otherwise = (ts,x:fs)

                        e = ([],[])

intercalatee :: [a] -> [[a]] -> [a]
intercalatee _ [] = []
intercalatee _ [y] = y
intercalatee xs (y:ys) = y : xs ++ intercalatee xs ys

-- flip :: (a -> b -> c) -> b -> a -> c