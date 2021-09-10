module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main


main :: IO ()
main = putStrLn "hello world"
-- main = interact (unlines . exercise . lines)

-- exercise :: [String] -> [String]
-- exercise = printTable
--          . project ["last", "first", "salary"]
--          . select "gender" "male"
--          . parseTable

-- | Parsing

-- * Exercise 1
parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2
printLine :: [Int] -> String

-- printLine xs = intercalate (replicate (maximum xs) '-') (replicate 2 "+")
printLine [] = ['+']
printLine (x : xs) = ['+'] ++ replicate x '-' ++ printLine xs

-- * Exercise 3
printField :: Int -> String -> String
printField num [] = []
printField num word = if (all isDigit word)
                        then 
                            replicate (num - (length word)) ' ' ++ word
                        else
                            word ++ replicate (num - (length word)) ' '
-- * Exercise 4
dataRow :: [(Int, String)]
dataRow = [(5, "Alice"), (6, "Allen"), (6, "female"), (6, "82000")]
               
printRow :: [(Int, String)] -> String
printRow [] = ['|']
printRow (x : xs) = ['|'] ++ uncurry printField x ++ printRow xs

-- * Exercise 5
dataTable :: Table
dataTable = [["first","last","gender","salary"],
             ["Alice","Allen","female","82000"],
             ["Bob","Baker","male","70000"],
             ["Carol","Clarke","female","50000"],
             ["Dan","Davies","male","45000"],
             ["Eve","Evans","female","275000"]]

columnWidths :: Table -> [Int]
columnWidths table = map maximum ((map (map length)) (transpose table))

-- * Exercise 6

-- printTable :: Table -> [String]
printTable table@(header:rows)
    = putStr (unlines
        (   [printLine (columnWidths table)] ++
            [printRow (zip (columnWidths table) ((map (map toUpper)) header))] ++
            [printLine (columnWidths table)] ++
            map printRow (map (zip (columnWidths dataTable)) rows) ++
            [printLine (columnWidths table)]
        ))
-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = undefined

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    = undefined
