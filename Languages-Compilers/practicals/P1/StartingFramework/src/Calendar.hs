module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import System.IO

instance Show DateTime where
    show = printDateTime

-- Exercise 6
data Calendar = Calendar { propId  :: PropId
                         , version :: Version
                         , events  :: [Event] }
    deriving (Eq, Ord, Show)

type PropId = String 
type Version = Float

-- some event properties are optional
data Event = Event { dtstart     :: DateTime
                   , dtend       :: DateTime
                   , location    :: Maybe Location
                   , uid         :: UID
                   , dtstamp     :: DateTime
                   , summary     :: Maybe Summary
                   , description :: Maybe Description }
    deriving (Eq, Ord, Show)

type UID = String
type Description = String
type Summary = String
type Location = String

-- Exercise 7
data Token = TokenBeginCalender | TokenProdId String | TokenVersion Float | TokenEndCalendar
           | TokenBeginEvent | TokenDTStart DateTime | TokenDTEnd DateTime 
           | TokenLocation Location | Token UID | TokenDTstamp DateTime 
           | TokenSummary Summary | TokenDescription Description | TokenEndEvent 
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = greedy anyToken
               
-- create token instances for Tokens
anyToken :: Parser Char Token 
anyToken = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

pTokenBeginCalendar :: Parser Char Token
pTokenBeginCalendar = TokenBeginCalender <$ token "BEGIN:CALENDAR"

pTokenProdId :: Parser Char Token
pTokenProdId = TokenProdId 

-- parseCalendar' :: Parser Char Calendar
-- parseCalendar' = Calendar            <$>
--                 (parseBeginCalendar  *>
--                  parseVersion        <*>
--                  parseProdId)        <*>
--                  many parseEvent     <*
--                  parseEndCalendar

-- parseBeginCalendar :: Parser Char String
-- parseBeginCalendar = token "BEGIN" <*> token ":" <*> token "VCALENDAR"

-- parseEndCalendar :: Parser Char String
-- parseEndCalendar = token "END" <*> token ":" <*> token "VCALENDAR"

-- parseVersion :: Parser Char Float
-- parseVersion = tokenParser "VERSION"

-- parseProdId :: Parser Char Float
-- parseProdId = tokenParser "PRODID"

-- parseEvent :: Parser Char Event
-- parseEvent = Event            <$>
--             (parseBeginEvent  *>
--              parseDTStart)    <*>
--              parseDTEnd       <*>
--             --  idk how to guarantee it can be ignored tbh, since it's an optional
--              parseLocation    <*>
--              parseUID         <*>
--              parseDTStamp     <*>
--              parseSummary     <*>
--              parseDescription

-- parseBeginEvent :: Parser Char String
-- parseBeginEvent = token "BEGIN" <*> token ":" <*> token "VEVENT"

-- parseEndEvent :: Parser Char String
-- parseEndEvent = token "END" <*> token ":" <*> token "VEVENT"

-- parseDTStart :: Parser Char DateTime
-- parseDTStart = tokenParser "DTSTART"

-- parseDTEnd :: Parser Char DateTime
-- parseDTEnd = tokenParser "DTEND"

-- parseLocation :: Parser Char (Maybe Location)
-- parseLocation = tokenParser "LOCATION"

-- parseUID :: Parser Char UID
-- parseUID = tokenParser "UID"

-- parseDTStamp :: Parser Char DateTime
-- parseDTStamp = tokenParser "DTSTAMP"

-- parseSummary :: Parser Char (Maybe Summary)
-- parseSummary = tokenParser "SUMMARY"

-- parseDescription :: Parser Char (Maybe Description)
-- parseDescription = tokenParser "DESCRIPTION"

-- tokenParser :: String -> Parser Char a
-- tokenParser xs = (token xs *> symbol ':' *>) $ many anySymbol

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
    x            <- openFile path ReadMode
    _            <- hSetNewlineMode x noNewlineTranslation
    fileContent  <- hGetContents x
    return $ recognizeCalendar fileContent

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar propId vers events) = "BEGIN:VCALENDAR" ++ newline ++ 
                                              "PROPID:" ++ propId ++ newline ++ 
                                              "VERSION:" ++ show vers ++ newline ++ 
                                              printEvents events ++ 
                                              "END:VCALENDAR" ++ newline
    where
        newline = "\r\n"

        printLocation :: Maybe Location -> String
        printLocation Nothing = ""
        printLocation (Just x) = "LOCATION:" ++ x ++ newline
        printSummary :: Maybe Summary -> String
        printSummary Nothing = ""
        printSummary (Just x) = "SUMMARY:" ++ x ++ newline
        printDescription :: Maybe Description -> String
        printDescription Nothing = ""
        printDescription (Just x) = "DESCRIPTION:" ++ x ++ newline

        printEvent :: Event -> String
        printEvent (Event dtstart dtend location uid dtstamp summary description) =
            "BEGIN:VEVENT" ++ newline ++
            "DTSTART:" ++ show dtstart ++ newline ++ 
            "DTEND:" ++ show dtend ++ newline ++
            printLocation location ++ 
            -- uid is already a string
            "UID:" ++ uid ++ newline ++
            "DTSTAMP:" ++ show dtstamp ++ newline ++
            printSummary summary ++ 
            printDescription description ++
            "END:VEVENT" ++ newline

        printEvents :: [Event] -> String 
        printEvents = concatMap printEvent 
