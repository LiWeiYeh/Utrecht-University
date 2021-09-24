module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * prod xs

concatt :: [[a]] -> [a]
concatt [] = []
concatt (xs:xxs) = xs ++ concatt xxs

andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) = x && and xs

orr :: [Bool] -> Bool
orr [] = True
orr (x:xs) | x = True
           | otherwise = orr xs

alll :: (a -> Bool) -> [a] -> Bool
alll _ [] = True
alll p (x:xs) | p x = alll p xs
              | otherwise = False

mapp :: (a -> b) -> [a] -> [b]
mapp _ [] = []
mapp f (x:xs) = f x : mapp f xs

interspersee :: Char -> [Char] -> [Char]
interspersee sep [] = []
interspersee sep [a] = [a]
interspersee sep (x:xs) = x : sep : interspersee sep xs

concatMapp :: (a -> [b]) -> [a] -> [b]
concatMapp f xs = concatt (mapp f xs)
                    where
                        concatt [] = []
                        concatt (x:xs) = x ++ concatt xs

unliness :: [String] -> String
unliness [] = []
-- unliness [a] = a
unliness (x:xs) = x ++ "\n" ++ unliness xs

filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr p (x:xs) | p x = x : filterr p xs
                 | otherwise = filterr p xs

partitionn _ [] = ([],[])
-- partitionn p xs = [filter p xs, filter (not . p) xs]
partitionn p (x:xs) | p x =       let (ts, fs) = partitionn p xs in (x:ts, fs)
                    | otherwise = let (ts, fs) = partitionn p xs in (ts, x:fs)

-- TODO
-- unzipp :: [(a,b)] -> ([a], [b])
-- unzipp [] = ([],[])
-- unzipp (x:xs) = (fst x : unzipp xs, snd x : unzipp xs)

insertt :: Ord a => a -> [a] -> [a]
insertt n [] = [n]
insertt n (x:xs) | n <= x = n : (x:xs)
                | otherwise = x : insertt n xs

sortt :: Ord a => [a] -> [a]
sortt [] = []
sortt [a] = [a]
sortt (x:xs) =  insertt x (sortt xs)

takee :: Int -> [a] -> [a]
takee _ [] = []
takee n (x:xs) | length (x:xs) < n  = x:xs
               | n == 0             = []
               | otherwise          = x : takee (n-1) xs

-- TODO
takeWhilee :: (a -> Bool) -> [a] -> [a]
takeWhilee _ [] = []
takeWhilee p (x:xs) | p x = x : takeWhilee p xs
                    | otherwise = []

groupp :: [Char] -> [[Char]]
groupp [] = []
groupp (x:xs) = case groupp xs of 
                    []                        -> [[x]]
                    (ys@(y:_):rs) | x == y    -> (x:ys) : rs
                                  | otherwise -> [x] : ys : rs

remSuccessiveduplicatess :: [Int] -> [Int]
remSuccessiveduplicatess [] = []
remSuccessiveduplicatess [a] = [a]
remSuccessiveduplicatess (x:y:ys) | x /= y = x : remSuccessiveduplicatess (y:ys)
                                  | otherwise = remSuccessiveduplicatess (y:ys)

-- nubb :: [a] -> [a]
nubb l = nubb' l []
    where
        nubb' [] _ = []
        nubb' (x:xs) seen 
            | x `elem` seen = nubb' xs seen
            | otherwise = x : nubb' xs (x:seen)

unionn :: Eq a => [a] -> [a] -> [a]
-- unionn _ ys = ys
-- unionn xs _ = xs
unionn xs ys = xs ++ help' ys xs
    where
        help' [] _ = []
        help' (z:zs) seen
            | z `elem` seen = help' zs seen
            | otherwise = z : help' zs seen

intersectt :: Eq a => [a] -> [a] -> [a]
intersectt xs ys = filterr (`elem` ys) xs

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast (x:xs) | null xs = Just x
                 | otherwise = maybeLast xs

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere x [] = [[x]]
insertEverywhere x xs@(y:ys) = (x:xs) : map (y:) (insertEverywhere x ys)

-- TODO: how does concatMap work here?
permutationss :: [a] -> [[a]]
permutationss [] = [[]]
permutationss (x:xs) = concatMapp (insertEverywhere x) (permutationss xs)

foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr f z [] = z
foldrr f z (x:xs) = f x (foldrr f z xs)

scanrr :: (a -> b -> b) -> b -> [a] -> [b]
scanrr f z [] = [z]
scanrr f z (x:xs) = 
    let rs@(r:_) = scanrr f z xs
    in f x r : scanrr f z xs

-- encodee :: 
encodee [] = []
encodee (x:xs) = case encodee xs of
                    []                          -> [(1,x)]
                    r@((i,y):ys) | x == y       -> (i+1,x) : ys
                                 | otherwise    -> (1,x) : r

decodee [] = []
decodee xs = concatMap (\(i,x) -> replicatee i x) xs
                where
                    replicatee 0 _ = []
                    replicatee i x = x: replicatee (i-1) x

splitAtt :: Int -> [a] -> ([a],[a])
splitAtt 0 xs = ([], xs)
splitAtt i [] = ([], [])
splitAtt i (x:xs) = let (ys, rest) = splitAtt (i-1) xs
                    in (x:ys, rest)

splitAlll :: Int -> [a] -> [[a]]
splitAlll i (x:xs) = case splitAtt i xs of 
                        (ys, [])    -> [ys]
                        (ys, rest)  -> ys : splitAlll i rest

zipWithh :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithh f (x:xs) (y:ys) = (f x y) : zipWithh f xs ys
zipWithh _ _ _ = []

transposee :: [[a]] -> [[a]]
transposee' (xs:xss) = zipWithh (:) xs (transposee' xss)
transposee' []       = repeat []
transposee [] = []
transposee xs = transposee' xs


-- Maximum segment sum

segments :: [Int] -> [[Int]]
segments = ([] :) . concatMap (tail . inits) . tails



-- Counting Trues



