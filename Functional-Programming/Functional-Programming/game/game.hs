module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

-- TA questions
-- Add position to objects?
-- type Position = (Float, Float)


-- Misc
data BulletType = Player 
                | Enemy
type BulletSpeed = Int
data Bullets = Bullets BulletType 

type ObjectSpeed = Float

data Mode = Survival 
          | Shoot

data SingleOrMultiPlayer = Single 
                         | Multi
-- Enemies
type Health = Int

-- Asteroid can split into multiple smaller Asteroids
type AsteroidLevel = Int
data Asteroid = Asteroid AsteroidLevel Speed

data UFO = UFO Speed Bullets 

-- Survival = can't shoot, only run
-- Shoot = can also shoot obviously


-- Player
type Lives = Int
-- Player is a spaceship. p1 has different color than p2
data SpaceShip = SpaceShip Lives Speed Bullets Position Color
-- Check if this is good
type P1 = SpaceShip
type P2 = SpaceShip
data Player = P1 | P2

data GameState = GameState { player :: Player
                           , asteroids :: [Asteroid]
                           , ufos :: [UFO]
                           , isPaused :: Bool
                           , mode :: Mode
                           , isSingleOrMultiPlayer :: SingleOrMultiPlayer
                           , score :: Int
}

move :: Float -> GameState -> GameState
