module Main where

-- import Data.Char(toUpper)
-- mkWelcome stylize year n = concat
-- [ stylize "Welcome to INFOFP in " ++ show year ++ "!\n\n""We have " ++ show n ++ " students.\n\n""So I will have to grade " ++ show (numExams n) ++ " exams...."]
-- where numExams m = 2 * m
-- main = putStrLn welcomeMsg
-- where capitalize = map toUpper
-- welcomeMsg = mkWelcome capitalize 2021 351

main = putStrLn "Lecture 1"

sumUpTo     :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n-1)

factorial   :: Int -> Int
factorial 0 = 1
factorial n = if n < 0 then error "negative number"
                    else n * factorial (n - 1)

greet       :: String -> String
greet name = "Hello, " ++ name ++ "!"

greet2       :: String -> String -> String
greet2 time name = "Good " ++ time ++ ", " ++ name ++ "!"
