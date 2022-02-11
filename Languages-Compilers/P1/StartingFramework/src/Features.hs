module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes

import System.IO
import Data.List

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ _ events) = length events 

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ _ events) = 
    filter (\(Event start end _ _ _ _ _) -> isEventHappening dt start end) events
    where
        isEventHappening :: DateTime -> DateTime -> DateTime -> Bool
        isEventHappening dt start end = (dateIsEarlier False dt end) && (dateIsEarlier True start dt)

        -- check if date is earlier, pass in a predicate which decides if the amount of seconds can be equal or not
        dateIsEarlier :: Bool -> DateTime -> DateTime -> Bool
        dateIsEarlier canEqual dt1 dt2 | canEqual = dt1 < dt2 || dt1 == dt2
                                    | otherwise = dt1 < dt2

checkOverlapping :: Calendar -> Bool
-- for every event's starttime, the events found by findEvent should only be 1, the event itself. if 
checkOverlapping calendar@(Calendar _ _ events) = any (\(Event start _ _ _ _ _ _) -> length(findEvents start calendar) /= 1) events

timeSpent :: String -> Calendar -> Int
-- timeSpent xs calendar@(Calendar _ _ events) = map (\(Event start end _ _ _ _ _) -> differenceInMinutesStartEnd start end) (filter (\(Event _ _ _ _ _ sum _) -> isSummaryEqual sum xs) events)
timeSpent xs calendar@(Calendar _ _ events) = foldr f 0 events
    where
        -- if the summary is equal, add to the sum
        f (Event start end _ _ _ sum _) r | isSummaryEqual sum xs = differenceInMinutesStartEnd start end + r
                                          | otherwise = r

        isSummaryEqual :: Maybe Summary -> String -> Bool
        isSummaryEqual Nothing _ = False
        isSummaryEqual (Just sum1) sum2 = sum1 == sum2

        differenceInMinutesStartEnd :: DateTime -> DateTime -> Int
        differenceInMinutesStartEnd (DateTime date1@(Date (Year yr1) (Month mth1) (Day dy1)) (Time (Hour hrs1) (Minute mins1) _) _) (DateTime date2@(Date (Year yr2) (Month mth2) (Day dy2)) (Time (Hour hrs2) (Minute mins2) _) _)
            | date1 == date2 = (hrs2 - hrs1) * 60 + (mins2 - mins1)
            | mth1 == mth2 = (dy2 - dy1) * 24 * 60 + (hrs2 - hrs1) * 60 + (mins2 - mins1)
            | otherwise = 0

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth yr mth (Calendar _ _ events) = 
    let maxDays = getDaysInMonth yr mth
        eventsByYearAndMonth = filterEventsByYearAndMonth yr mth events
    in
        printRowLine ++
        printWeekRowWithDayRow 1 maxDays ++ "\n" ++
        printEventsByWeek 1 maxDays eventsByYearAndMonth ++ 
        printRowLine ++
        printWeekRowWithDayRow 2 maxDays ++ "\n" ++
        printEventsByWeek 2 maxDays eventsByYearAndMonth ++ 
        printRowLine ++
        printWeekRowWithDayRow 3 maxDays ++ "\n" ++
        printEventsByWeek 3 maxDays eventsByYearAndMonth ++ 
        printRowLine ++
        printWeekRowWithDayRow 4 maxDays ++ "\n" ++
        printEventsByWeek 4 maxDays eventsByYearAndMonth ++ 
        printRowLine ++
        -- The helper functions of these functions are different, due to having different styles of computing with max days
        printWeekRowWithDayRow 5 maxDays ++ "\n" ++
        printEventsByWeek 5 maxDays eventsByYearAndMonth

    where
        printRowLine :: String
        printRowLine = "------------------+------------------+------------------+------------------+------------------+------------------+------------------+\n"

        printEventsByWeek :: Int -> Int -> [Event] -> String
        printEventsByWeek week maxDays events | week < 5  = let filteredEventsByWeek = filterEventsByWeek week events   
                                                                maxEvents = getMaxEventsDayByWeek $ filterEventsByWeek week events      
                                                            in
                                                                printEventsByWeek' week 1 maxEvents filteredEventsByWeek
                                              | otherwise = let filteredEventsByWeek = filterEventsByWeek week events
                                                                maxEvents = getMaxEventsDayByWeek filteredEventsByWeek   
                                                            in 
                                                                printEventsByLastWeek' 29 maxDays maxEvents filteredEventsByWeek
            where
                printEventsByWeek' :: Int -> Int -> Int -> [Event] -> String
                printEventsByWeek' week dayNr maxEvents events 
                    | maxEvents <= 0 = ""
                    | dayNr == 7 = let foundEventIfAny = findFirstEventByDay (Day (dayNr+(7*(week-1)))) events
                                   in 
                                       printEventIfAny foundEventIfAny ++ "\n" ++ printEventsByWeek' week 1 (maxEvents-1) (removeFromEventsIfFound foundEventIfAny events) 
                    | otherwise = let foundEventIfAny = findFirstEventByDay (Day (dayNr+(7*(week-1)))) events
                                  in 
                                      printEventIfAny foundEventIfAny ++ printEventsByWeek' week (dayNr+1) maxEvents (removeFromEventsIfFound foundEventIfAny events) 

                printEventsByLastWeek' :: Int -> Int -> Int -> [Event] -> String
                printEventsByLastWeek' dayNr maxDays maxEvents events 
                    | maxEvents <= 0 = ""
                    | dayNr == maxDays = let foundEventIfAny = findFirstEventByDay (Day dayNr) events 
                                         in 
                                             printEventIfAny foundEventIfAny ++ "\n" ++ printEventsByLastWeek' 29 maxDays (maxEvents-1) (removeFromEventsIfFound foundEventIfAny events)
                    | otherwise = let foundEventIfAny = findFirstEventByDay (Day dayNr) events 
                                  in 
                                      printEventIfAny foundEventIfAny ++ printEventsByLastWeek' (dayNr + 1) maxDays maxEvents (removeFromEventsIfFound foundEventIfAny events)

                removeFromEventsIfFound :: Maybe Event -> [Event] -> [Event]
                removeFromEventsIfFound Nothing xs = xs
                removeFromEventsIfFound (Just x) xs = filter (\e -> x /= e) xs

                printEventIfAny :: Maybe Event -> String
                printEventIfAny Nothing = printEmpty
                printEventIfAny (Just (Event dt1@(DateTime date1 (Time (Hour hrs1) (Minute mins1) _) _) dt2@(DateTime date2@(Date (Year yr) (Month mth) (Day dy)) (Time (Hour hrs2) (Minute mins2) _) _) _ _ _ _ _ ))
                    | dt1 >= dt2 = printEmpty
                    | date1 < date2 = mapExtraZeros2 hrs1 ++ ":" ++ mapExtraZeros2 mins1 ++ " - " ++ mapExtraZerosN 4 yr ++ "/" ++ mapExtraZeros2 mth ++ "/" ++ mapExtraZeros2 dy ++ "|"
                    | otherwise = mapExtraZeros2 hrs1 ++ ":" ++ mapExtraZeros2 mins1 ++ " - " ++ mapExtraZeros2 hrs2 ++ ":" ++ mapExtraZeros2 mins2 ++ replicate 5 ' ' ++ "|"
                printEmpty :: String
                printEmpty = replicate 18 ' ' ++ "|"

        printWeekRowWithDayRow :: Int -> Int -> String
        printWeekRowWithDayRow week maxDays | week < 5 = printWeek week 1
                                            | otherwise = printLastWeek 29
            where
                printWeek :: Int -> Int -> String
                printWeek week passedDays | passedDays > 7 = ""
                                          | otherwise = printDayRow (Day (passedDays + 7*(week-1))) ++ printWeek week (passedDays + 1)

                printLastWeek :: Int -> String
                printLastWeek day | day > maxDays = ""
                                  | otherwise = printDayRow (Day day) ++ printLastWeek (day + 1)

        printDayRow :: Day -> String
        printDayRow (Day dy) = show dy ++ replicate (18 - length (show dy)) ' ' ++ "|"

        findFirstEventByDay :: Day -> [Event] -> Maybe Event
        findFirstEventByDay _ [] = Nothing
        findFirstEventByDay (Day day) (event@(Event (DateTime (Date _ _ (Day day')) _ _) _ _ _ _ _ _):xs)
            | day > day' = Nothing
            | day == day' = Just event
            | otherwise = findFirstEventByDay (Day day) xs

        filterEventsByWeek :: Int -> [Event] -> [Event]
        filterEventsByWeek n = filter (\(Event (DateTime (Date _ _ (Day day')) _ _) _ _ _ _ _ _) -> (day' >= 7*(n-1) + 1) && (day' <= 7*n))

        insertEvent :: Event -> [Event] -> [Event]
        insertEvent e [] = [e]
        insertEvent e (x:xs) | e < x = e : (x:xs)
                             | otherwise = x : insertEvent e xs 
        sortEvents :: [Event] -> [Event]
        sortEvents [] = []
        sortEvents (x:xs) = insertEvent x xs

        -- !!! ONLY use this function when input is an already filtered events, with only the specific event of said week
        getMaxEventsDayByWeek :: [Event] -> Int
        getMaxEventsDayByWeek [] = 0
        getMaxEventsDayByWeek events = maximum $ map length (group $ map (\(Event (DateTime (Date _ _ day') _ _) _ _ _ _ _ _) -> day') (sortEvents events))

        filterEventsByYearAndMonth :: Year -> Month -> [Event] -> [Event]
        filterEventsByYearAndMonth yr mth = 
            filter (\(Event (DateTime (Date yr' mth' _) _ _) _ _ _ _ _ _) -> yr == yr' && mth == mth')

        isLeapYear :: Year -> Bool
        isLeapYear (Year yr) 
            | yr `mod` 100 == 0 && yr `mod` 400 /= 0 = False
            | yr `mod` 4 == 0 = True
            | otherwise = False

        getDaysInMonth :: Year -> Month -> Int
        getDaysInMonth year (Month mth) | mth == 1 || mth == 3 || mth == 5 || mth == 7 || mth == 8 || mth == 10 || mth == 12 = 31
                                        | mth == 4 || mth == 4 || mth == 6 || mth == 9 || mth == 11 = 30
                                        | mth == 2 && isLeapYear year == False = 28
                                        -- when it is feb && isLeapYear
                                        | otherwise = 29
