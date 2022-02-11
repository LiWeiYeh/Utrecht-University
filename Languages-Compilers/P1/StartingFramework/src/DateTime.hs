module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Char

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)

-- type Parser s r = [s] -> [(r, [s])]

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseIsUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
    where
        parseYear :: Parser Char Year
        parseYear = (\a b c d -> Year $ read [a, b, c, d]) <$> digit <*> digit <*> digit <*> digit

        parseMonth :: Parser Char Month
        parseMonth = (\a b -> Month $ read [a, b]) <$> digit <*> digit

        parseDay :: Parser Char Day
        parseDay = (\a b -> Day $ read [a, b]) <$> digit <*> digit

parseTime :: Parser Char Time
parseTime = Time <$> (symbol 'T' *> parseHour) <*> parseMinute <*> parseSecond
    where
        parseHour :: Parser Char Hour
        parseHour = (\a b -> Hour $ read [a, b]) <$> digit <*> digit

        parseMinute :: Parser Char Minute
        parseMinute = (\a b -> Minute $ read [a, b]) <$> digit <*> digit

        parseSecond :: Parser Char Second
        parseSecond = (\a b -> Second $ read [a, b]) <$> digit <*> digit

parseIsUtc :: Parser Char Bool
parseIsUtc = (succeed True <* symbol 'Z') <|> succeed False

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run parser xs = f $ parse parser xs
    where
        -- if remainder is empty, return Just result, else recursively check the next elements. If none found, return Nothing
        f [] = Nothing
        f ((r, zs):xs)
            | null zs = Just r
            | otherwise = f xs

-- Exercise 3
-- output: 19970610T172345Z
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hours) (Minute minutes) (Second seconds)) z) = 
    mapExtraZerosN 4 year ++ 
    mapExtraZeros2 month ++ 
    mapExtraZeros2 day ++ 
    "T" ++ 
    mapExtraZeros2 hours ++ 
    mapExtraZeros2 minutes ++ 
    mapExtraZeros2 seconds ++ 
    isUtc z
    where
        isUtc :: Bool -> String
        isUtc timezone | timezone = "Z"
                       | otherwise = ""


mapExtraZerosN :: Int -> Int -> String
-- n being amount of characters e.g. year is 4 characters, others are 2
mapExtraZerosN n x = replicate (n - length (show x)) '0' ++ show x

mapExtraZeros2 :: Int -> String
mapExtraZeros2 = mapExtraZerosN 2

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime date time _) = isValidDate date && isValidTime time

isValidDate :: Date -> Bool
isValidDate (Date year month day) = isValidYear year && isValidMonth month && isValidDay year month day
    where
        isValidYear :: Year -> Bool
        isValidYear (Year yr) = yr >= 0 && yr < 10000
        isValidMonth :: Month -> Bool
        isValidMonth (Month mth) = mth >= 1 && mth <= 12
        isValidDay :: Year -> Month -> Day -> Bool
        isValidDay year month day = isValidDayByMonth (isLeapYear year) month day
            where
                isLeapYear :: Year -> Bool
                isLeapYear (Year yr) 
                    | yr `mod` 100 == 0 && yr `mod` 400 /= 0 = False
                    | yr `mod` 4 == 0 = True
                    | otherwise = False

                isValidDayByMonth :: Bool -> Month -> Day -> Bool
                isValidDayByMonth isLeapYear (Month mth) (Day dy) 
                    | mth == 1 || mth == 3 || mth == 5 || mth == 7 || mth == 8 || mth == 10 || mth == 12 = dy <= 31
                    | mth == 4 || mth == 4 || mth == 6 || mth == 9 || mth == 11 = dy <= 30
                    | mth == 2 && isLeapYear == False = dy <= 28
                    | otherwise = dy <= 29 

isValidTime :: Time -> Bool
isValidTime (Time hour minute second) = isValidHour hour && isValidMinute minute && isValidSecond second
    where
        isValidHour :: Hour -> Bool
        isValidHour (Hour hrs) = hrs >= 0 && hrs <= 23
        isValidMinute :: Minute -> Bool
        isValidMinute (Minute mins) = mins >= 0 && mins <= 59
        isValidSecond :: Second -> Bool
        isValidSecond (Second secs) = secs >= 0 && secs <= 59
