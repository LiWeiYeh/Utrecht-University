{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

exampleRose =   MkRose 1
                [
                    MkRose 2 
                        [
                            MkRose 3 [], 
                            MkRose 6 []
                        ], 
                    MkRose 4 
                        [
                            MkRose 5 []
                        ]
                ]

-- Exercise 1

root :: Rose a -> a
root (MkRose x _) = x

children :: Rose a -> [Rose a]
children (MkRose _ children) = children

-- Exercise 2

size :: Rose a -> Int
size (MkRose value []) = 1
size (MkRose value children) = 1 + sum (map size children)

leaves :: Rose a -> Int
leaves (MkRose value []) = 1
leaves (MkRose value children) = sum (map leaves children)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"
    
-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

exampleBoard = (
    (X, X, O),
    (O, O, B),
    (B, B, X))

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((r1c1,r1c2,r1c3), 
           (r2c1,r2c2,r2c3),
           (r3c1,r3c2,r3c3)) = ((r1c1,r2c1,r3c1),
                                (r1c2,r2c2,r3c2),
                                (r1c3,r2c3,r3c3))

diagonals :: Board -> (Row, Row)
diagonals ((r1c1,_   ,r1c3), 
           (_   ,r2c2,_   ),
           (r3c1,_   ,r3c3)) = ((r1c1,r2c2,r3c3),
                                (r1c3,r2c2,r3c1))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B),
              (B, B, B),
              (B, B, B))

-- Exercise 7

printBoard :: Board -> String
printBoard (r1, 
            r2,
            r3) = printRow r1 ++ "\n" ++ 
                  printLine ++ "\n" ++
                  printRow r2 ++ "\n" ++ 
                  printLine ++ "\n" ++
                  printRow r3 ++ "\n" ++ 
                  printLine

printRow :: Row -> String
printRow (c1, c2, c3) = show c1 ++ "|" ++ show c2 ++ "|" ++ show c3
printLine :: String
printLine = "-+-+-"

-- | Move generation
             
-- Exercise 8
             
moves :: Player -> Board -> [Board]
moves p (r1,r2,r3) = func 1 r1 ++
                     func 2 r1 ++
                     func 3 r1 ++
                     func 4 r2 ++ 
                     func 5 r2 ++ 
                     func 6 r2 ++
                     func 7 r3 ++
                     func 8 r3 ++
                     func 9 r3
                        where 
                            func x (c1,c2,c3) | x == 1 = if c1 == B 
                                                         then [((sign,c2,c3),r2,r3)]
                                                         else []
                                              | x == 2 = if c2 == B 
                                                         then [((c1,sign,c3),r2,r3)]
                                                         else []
                                              | x == 3 = if c3 == B 
                                                         then [((c1,c2,sign),r2,r3)]
                                                         else []
                                              | x == 4 = if c1 == B 
                                                         then [(r1,(sign,c2,c3),r3)]
                                                         else []
                                              | x == 5 = if c2 == B 
                                                         then [(r1,(c2,sign,c3),r3)]
                                                         else []
                                              | x == 6 = if c3 == B 
                                                         then [(r1,(c1,c2,sign),r3)]
                                                         else []
                                              | x == 7 = if c1 == B 
                                                         then [(r1,r2,(sign,c2,c3))]
                                                         else []
                                              | x == 8 = if c2 == B 
                                                         then [(r1,r2,(c1,sign,c3))]
                                                         else []
                                              | x == 9 = if c3 == B 
                                                         then [(r1,r2,(c1,c2,sign))]
                                                         else []
                            sign = symbol p

-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner (r1,r2,r3) = hasWinnerRows (r1,r2,r3) (verticals (r1,r2,r3)) (diagonals (r1,r2,r3))

hasWinnerRow :: Row -> Maybe Player
hasWinnerRow (a,b,c) | a == b && b == c && a == symbol P1 = Just P1
                     | a == b && b == c && a == symbol P2 = Just P2
                     | otherwise = Nothing

hasWinnerRows (a,b,c) (d,e,f) (g,h) | hasWinnerRow a /= Nothing = hasWinnerRow a
                                    | hasWinnerRow b /= Nothing = hasWinnerRow b
                                    | hasWinnerRow c /= Nothing = hasWinnerRow c
                                    | hasWinnerRow d /= Nothing = hasWinnerRow d
                                    | hasWinnerRow e /= Nothing = hasWinnerRow e
                                    | hasWinnerRow f /= Nothing = hasWinnerRow f
                                    | hasWinnerRow g /= Nothing = hasWinnerRow g
                                    | hasWinnerRow h /= Nothing = hasWinnerRow h
                                    | otherwise = Nothing

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree p board | hasWinner board /= Nothing = MkRose board []
                 | otherwise = MkRose board (map (\board' -> gameTree (nextPlayer p) board') (moves p board))



-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Exercise 12

exampleBoard2 = ((X, B, B),
                 (O, B, O),
                 (X, B, B))

minimax :: Player -> Rose Board -> Rose Int
minimax p roseboard = minimax' p roseboard
            where
                minimax' currentPlayer (MkRose a []) | hasWinner a == Just p = MkRose 1 []
                                                     | hasWinner a == Nothing = MkRose 0 []
                                                     | otherwise = MkRose (-1) []
                minimax' currentPlayer (MkRose _ children) | p == currentPlayer = MkRose (maximum' [a | (MkRose a _) <- childRoseInts]) childRoseInts
                                                           | otherwise          = MkRose (minimum' [a | (MkRose a _) <- childRoseInts]) childRoseInts
                    where
                        childRoseInts :: [Rose Int]
                        childRoseInts = map (minimax' (nextPlayer currentPlayer)) children


-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = minimum'' 1
            where
                minimum'' :: Int -> [Int] -> Int
                minimum'' _ ((-1):_) = -1
                minimum'' acc [] = acc
                minimum'' acc (y:ys) | y < acc = minimum'' y ys
                                        | otherwise = minimum'' acc ys

maximum' :: [Int] -> Int
maximum' = maximum'' (-1)
            where
                maximum'' :: Int -> [Int] -> Int
                maximum'' _ (1:_) = 1
                maximum'' acc [] = acc
                maximum'' acc (y:ys) | y > acc = maximum'' y ys
                                     | otherwise = maximum'' acc ys

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove p board = bestBoard (root scoresRose') (children boardsRose') (children scoresRose')
                    where 
                        bestBoard :: Int -> [Rose Board] -> [Rose Int] -> Maybe Board
                        bestBoard _ [] [] = Nothing 
                        bestBoard x ((MkRose valueBoard _):xs) ((MkRose valueScore _):ys) | x == valueScore = Just valueBoard
                                                                                          | otherwise = bestBoard x xs ys
                        boardsRose' = gameTree p board
                        scoresRose' = minimax p boardsRose'

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType 
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b) 
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y