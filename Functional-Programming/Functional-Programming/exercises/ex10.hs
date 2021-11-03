{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Foldable
import Data.List

import Test.QuickCheck

main :: IO ()
main = undefined


instance (Enum a, Bounded a, Show  a) => Show (a -> Bool) where
  show f = intercalate "\n" (map (\x -> "f " ++ show x ++ " = " ++ show (f x)) [minBound .. maxBound])

runTests :: IO ()
runTests = do
  putStrLn "\nExercise 14.1"
  quickCheck (propFilterNoLonger      :: (Bool -> Bool) -> [Bool] -> Bool)
  quickCheck (propFilterAllSatisfy    :: (Bool -> Bool) -> [Bool] -> Bool)
  quickCheck (propFilterAllElements   :: (Bool -> Bool) -> [Bool] -> Bool)
  quickCheck (propFilterCorrect       :: (Bool -> Bool) -> [Bool] -> Bool)
  putStrLn "\nExercise 14.2"
  quickCheck (propMapLength :: (Bool -> Bool) -> [Bool] -> Bool)
  putStrLn "\nExercise 14.3"
  quickCheck $ once (propPermsLength   :: [Int] -> Bool)
  -- quickCheck $ once (propPermsArePerms :: [Int] -> Bool)
  -- quickCheck $ once (propPermsCorrect  :: [Int] -> Bool)
  -- putStrLn "\nExercise 14.5"
  -- quickCheck (forAll genBSTI isSearchTree)    -- Use forAll to use custom generator
  -- quickCheck (forAll genBSTI propInsertIsTree)
  -- quickCheck (forAll genBSTI propInsertIsTreeWrong)

propFilterNoLonger      :: (a -> Bool) -> [a] -> Bool
propFilterNoLonger p xs = length (filter p xs) <= length xs

propFilterAllSatisfy    :: (a -> Bool) -> [a] -> Bool
propFilterAllSatisfy p xs = all p $ filter p xs

propFilterAllElements   :: Eq a => (a -> Bool) -> [a] -> Bool
propFilterAllElements p xs = all (\x -> x `elem` xs) $ filter p xs

propFilterCorrect       :: Eq a => (a -> Bool) -> [a] -> Bool
propFilterCorrect p xs = all (\prop -> prop p xs) [ propFilterNoLonger
                                                  , propFilterAllSatisfy
                                                  , propFilterAllElements
                                                  ]

-- propFilterKeepsOrder    :: Eq a => (a -> Bool) -> [a] -> Bool
-- propFilterKeepsOrder p [] = filter p xs == [] 
-- propFilterKeepsOrder p xs = filter p xs

propMapLength :: (a -> b) -> [a] -> Bool
propMapLength f xs = (length $ map f xs) == (length xs)

propPermsLength :: [a] -> Bool
propPermsLength xs = length (permutations xs) == factorial (length xs)
  where
    factorial :: Int -> Int
    factorial n | n > 1 = n * factorial (n-1)
                | otherwise = 1

isPerm :: Eq a => [a] -> [a] -> Bool
isPerm xs ys = all (sameNumOcc xs ys) xs && all (sameNumOcc xs ys) ys
  where
    sameNumOcc xs ys x = numOcc xs x == numOcc ys x
    numOcc xs x = length . filter (== x) $ xs
    
    
propPermsArePerms :: Eq a => [a] -> Bool
propPermsArePerms xs = all (isPerm xs) $ permutations xs

-- propPermsCorrect :: Eq a => [a] -> Bool
-- propPermsCorrect xs = all (isPerm xs) $ permutations xs





-- ex 5
data Tree a = Branch a (Tree a) (Tree a) 
            | Leaf

isSearchTree :: Tree a -> Bool




