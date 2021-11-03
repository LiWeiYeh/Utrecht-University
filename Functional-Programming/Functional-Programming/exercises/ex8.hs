module Main where

import Data.Foldable
import Data.List

main :: IO ()
-- main = 
    -- do
    -- putStrLn "Filenames: "
    -- paths <- askFiles

    -- where askFiles = 
    --     do 
    --         path <- getLine
    --         case path of
    --             "" -> return []
    --             _  -> return (path:askFiles) 

-- main = do 
--     tree <- readTree
--     putStrLn "Give a number: "
--     q <- getChar
--     q' <- read [n]
--     isInTree <- findTree tree q'
--     case isInTree of
--         True  -> putStrLn "The number appears in the Tree"
--         False -> putStrLn "The number does not appear in the Tree"
--         _ -> ()

main = undefined


returnMultiples = do
    putStrLn "Give a number: "
    n <- getChar
    mapM_ (putStrLn . f (read [n]) ) [1..10]

    where
        f n k = unwords [ show k, "*", show n, "=", show (n*k) ]

printsLinesAndWordsFile = do
    putStrLn "What is the filename?"
    path <- getLine
    fileContents <- readFile path
    let n = length . lines $ fileContents
        m = length . words $ fileContents
    putStrLn ("number of lines: " ++ show n)
    putStrLn ("number of words: " ++ show m) 

-- foo :: [FilePath] -> FilePath -> IO ()
-- foo paths path = 
data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show,Read,Eq)

readTree :: IO (Tree Int)
readTree = do
    putStrLn "What is the tree filename?"
    path <- getLine
    contents <- readFile path
    return (read contents)

-- findTree (Leaf a) n     | n == a = True
--                         | otherwise = False
-- findTree (Node l x r) n | n == x = True
--                         | n <  x = findTree l
--                         | otherwise = findTree r