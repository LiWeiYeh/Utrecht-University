module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Data.Maybe

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
               deriving (Show, Eq)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file that can be found in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Interpreter.Empty   , '.' )
                 , (Interpreter.Lambda  , '\\')
                 , (Interpreter.Debris  , '%' )
                 , (Interpreter.Asteroid, 'O' )
                 , (Interpreter.Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace s = 
    show largestPos ++ "\n" ++ 
    foldr f "" positions
    where
        positions :: [Pos]
        positions = L.keys s      -- Map.keys orders get the keys in ascending order

        largestPos :: Pos
        largestPos = last positions    -- get the last element of the positions

        -- prints characters untill it has reached the largest x pos
        f x r = printLine x ++ (if isLastElementRow x then "\n" else "") ++ r
            where
                printLine :: Pos -> String
                -- finds the character in the map, finds the correct Char
                -- in the table and converts it into a [Char] = String 
                printLine = (\ele -> [fromJust (lookup ele contentsTable)]) . (s L.!)
                isLastElementRow :: Pos -> Bool
                -- checks if it's the column count
                isLastElementRow pos = (snd pos == snd largestPos)



-- These three should be defined by you
type Ident = Model.Ident
type Commands = [Cmd]
type Heading = Interpreter.Direction

data Direction = North
               | East
               | South
               | West
               deriving (Show, Eq)

type Environment = Map Interpreter.Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s 
    | checkProgram p = foldr f L.empty rules
    | otherwise = error "checkProgram Failure"
    where
        -- insert a new key value pair in the rules 
        f (Rule id cmds) = L.insert id cmds
        -- TODO: fix Parser.y
        p@(Program rules) = Program []
        -- p@(Program rules) = Parser.parseTokens Lexer.alexScanTokens s

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState space pos heading []) =
    Done space pos heading
step env arrowstate@(ArrowState space pos heading stack@(x:xs)) = 
    case x of
        Go -> 
            case L.lookup newPos space of
                (Just Interpreter.Empty)  -> Ok (ArrowState space newPos heading xs)
                (Just Interpreter.Lambda) -> Ok (ArrowState space newPos heading xs)
                (Just Interpreter.Debris) -> Ok (ArrowState space newPos heading xs)
                _                         -> doNothing xs        

        Take -> 
            case L.lookup pos space of
                (Just Interpreter.Lambda) -> Ok (ArrowState (L.insert pos Interpreter.Empty space) pos heading xs)
                (Just Interpreter.Debris) -> Ok (ArrowState (L.insert pos Interpreter.Empty space) pos heading xs)
                _                         -> doNothing xs

        Mark -> Ok (ArrowState (L.insert pos Interpreter.Lambda space) pos heading xs)

        Nothing' -> doNothing xs

        Turn dir -> Ok (ArrowState space pos (updateHeading heading dir) xs)

        Case dir alts -> 
            -- find if there's an item at the position with the next direction or a boundary
            let contents = case L.lookup (updatePosWithDir pos dir heading) space of
                    Nothing -> Interpreter.Boundary
                    Just c -> c 
            in 
                let cmdss = [cmds | (Alt pat cmds) <- alts, (isSameContent contents pat)] 
                in
                    case cmdss of 
                        [] -> Fail "Failed to find contents"
                        -- add cmds to the stack
                        (y:_) -> Ok (ArrowState space pos heading (y ++ xs))

        Ident id -> 
            case L.lookup id env of
                Nothing -> Fail ("failed to find the command" ++ id)
                Just cmds -> Ok (ArrowState space pos heading (cmds ++ xs))

    where
        updatePos :: Pos -> Heading -> Pos
        updatePos (x,y) North = (x-1,  y)
        updatePos (x,y) East  = (x,  y+1)
        updatePos (x,y) South = (x+1,  y)
        updatePos (x,y) West  = (x,  y-1)

        updateHeading :: Heading -> Model.Direction -> Heading
        updateHeading North Model.Right = East
        updateHeading North Model.Left  = West
        updateHeading East  Model.Right = South
        updateHeading East  Model.Left  = North
        updateHeading South Model.Right = West
        updateHeading South Model.Left  = East
        updateHeading West  Model.Right = North
        updateHeading West  Model.Left  = South
        updateHeading h     Model.Front = h

        newPos :: Pos
        newPos = updatePos pos heading
        
        doNothing :: [Cmd] -> Step
        doNothing cmds = Ok (ArrowState space pos heading cmds)

        updatePosWithDir :: Pos -> Model.Direction -> Heading -> Pos
        updatePosWithDir pos dir h =  updatePos pos $ updateHeading h dir

        isSameContent :: Contents -> Pat -> Bool
        isSameContent Interpreter.Empty    Model.Empty    = True
        isSameContent Interpreter.Lambda   Model.Lambda   = True
        isSameContent Interpreter.Debris   Model.Debris   = True
        isSameContent Interpreter.Asteroid Model.Asteroid = True
        isSameContent Interpreter.Boundary Model.Boundary = True
        isSameContent _                    Underscore     = True
        isSameContent _                    _              = False
