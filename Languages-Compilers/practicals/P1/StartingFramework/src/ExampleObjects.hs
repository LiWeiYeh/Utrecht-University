module ExampleObjects where

import DateTime
import Calendar
import Features


exampleDateTime :: DateTime
exampleDateTime = DateTime (Date (Year 1999) (Month 4) (Day 8)) (Time (Hour 02) (Minute 06) (Second 54)) False

exampleDateTime2 :: DateTime
exampleDateTime2 = DateTime (Date (Year 1999) (Month 4) (Day 8)) (Time (Hour 02) (Minute 06) (Second 55)) True

exampleCalendar = Calendar "/" 2.0 [ Event exampleDateTime exampleDateTime (Just "someLoc1") "wefhiohiowef@hr.nl" exampleDateTime (Just "someSumm1") (Just "someDescr1") 
                                   , Event exampleDateTime exampleDateTime (Just "someLoc2") "qwefgqwfgqwgf@hr.nl" exampleDateTime (Just "someSumm2") (Just "someDescr2") 
                                   , Event exampleDateTime exampleDateTime (Just "someLoc3") "asdgasdgasdggw@hr.nl" exampleDateTime (Just "someSumm3") (Just "someDescr3") ]

exampleCalendarJuly = Calendar "/" 2.0 [ Event exampleDateTimeJuly1 exampleDateTimeJuly11 (Just "someLoc1") "wefhiohiowef@hr.nl" exampleDateTimeJuly1 (Just "someSumm1") (Just "someDescr1") 
                                       , Event exampleDateTimeJuly2 exampleDateTimeJuly22 (Just "someLoc2") "qwefgqwfgqwgf@hr.nl" exampleDateTimeJuly1 (Just "someSumm2") (Just "someDescr2") 
                                       , Event exampleDateTimeJuly3 exampleDateTimeJuly33 (Just "someLoc3") "asdgasdgasdggw@hr.nl" exampleDateTimeJuly1 (Just "someSumm3") (Just "someDescr3") 
                                       , Event exampleDateTimeJuly4 exampleDateTimeJuly44 (Just "someLoc3") "asdgasdgasdggw@hr.nl" exampleDateTimeJuly1 (Just "someSumm3") (Just "someDescr3")
                                        -- 5 and 6 overlap
                                       , Event exampleDateTimeJuly5 exampleDateTimeJuly55 (Just "someLoc3") "asdgasdgasdggw@hr.nl" exampleDateTimeJuly1 (Just "someSumm3") (Just "someDescr3") 
                                       , Event exampleDateTimeJuly6 exampleDateTimeJuly66 (Just "someLoc3") "asdgasdgasdggw@hr.nl" exampleDateTimeJuly1 (Just "someSumm3") (Just "someDescr3") 
                                       ]

exampleDateTimeJuly0 = DateTime (Date (Year 1999) (Month 7) (Day 31)) (Time (Hour 05) (Minute 06) (Second 54)) False

exampleDateTimeJuly1 = DateTime (Date (Year 1999) (Month 7) (Day 3)) (Time (Hour 05) (Minute 06) (Second 54)) False
exampleDateTimeJuly11 = DateTime (Date (Year 1999) (Month 7) (Day 3)) (Time (Hour 08) (Minute 06) (Second 54)) False
-- exampleDateTimeJuly111 = DateTime (Date (Year 1999) (Month 7) (Day 3)) (Time (Hour 06) (Minute 06) (Second 54)) False


exampleDateTimeJuly2 = DateTime (Date (Year 1999) (Month 7) (Day 10)) (Time (Hour 07) (Minute 06) (Second 54)) False
exampleDateTimeJuly22 = DateTime (Date (Year 1999) (Month 7) (Day 17)) (Time (Hour 05) (Minute 12) (Second 54)) False

exampleDateTimeJuly3 = DateTime (Date (Year 1999) (Month 7) (Day 1)) (Time (Hour 02) (Minute 06) (Second 54)) False
exampleDateTimeJuly33 = DateTime (Date (Year 1999) (Month 7) (Day 3)) (Time (Hour 03) (Minute 06) (Second 54)) False

exampleDateTimeJuly4 = DateTime (Date (Year 1999) (Month 7) (Day 29)) (Time (Hour 02) (Minute 06) (Second 54)) False
exampleDateTimeJuly44 = DateTime (Date (Year 1999) (Month 7) (Day 29)) (Time (Hour 02) (Minute 12) (Second 54)) False

exampleDateTimeJuly5 = DateTime (Date (Year 1999) (Month 7) (Day 31)) (Time (Hour 03) (Minute 06) (Second 54)) False
exampleDateTimeJuly55 = DateTime (Date (Year 1999) (Month 7) (Day 31)) (Time (Hour 09) (Minute 06) (Second 54)) False

exampleDateTimeJuly6 = DateTime (Date (Year 1999) (Month 7) (Day 31)) (Time (Hour 03) (Minute 07) (Second 54)) False
exampleDateTimeJuly66 = DateTime (Date (Year 1999) (Month 7) (Day 31)) (Time (Hour 09) (Minute 22) (Second 54)) False

printExampleCalendarJuly :: String
printExampleCalendarJuly = ppMonth (Year 1999) (Month 7) exampleCalendarJuly