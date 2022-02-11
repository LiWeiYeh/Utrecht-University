-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Player
import Asteroids
import Bullets
import Utilities

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState player@(SpaceShip _ pPos pVel _ _ _) asteroids _ ufos score elapsedTime spawnTimerAst spawnTimerUfo shootTimerUfo playstate _) = 
  case playstate of 
    IsPlaying
      | spawnTimerAst + secs > nO_SECS_BETWEEN_CYCLES * 3
      -> 
        do
          -- initializes values for the creation of Asteroids
          r <- randomRIO (20, 50) :: IO Float
          asteroidPos <- getAsteroidPos r
          asteroidVel <- getAsteroidVel asteroidPos pPos elapsedTime

          return $ basicFuncs
                --  add asteroids to gamestate
                 $ gstate { spawnTimerAst = 0, asteroids = addAsteroid asteroidPos asteroidVel r asteroids }
      | spawnTimerUfo + secs > nO_SECS_BETWEEN_CYCLES * 6.5
      ->
        do
          ufoPos <- getUfoPos
          return $ basicFuncs
                  --  add ufos to gamestate
                 $ gstate { spawnTimerUfo = 0, ufos = addUfo ufoPos (1, 0.5) 40 ufos }

      | shootTimerUfo + secs > nO_SECS_BETWEEN_CYCLES * 5
      ->
        do
          return $ basicFuncs
                 -- shoots bullets from ufo
                 $ ufosShoot
                 $ gstate { shootTimerUfo = 0 }
      | otherwise
      ->
        return $ basicFuncs gstate

    IsPaused
      -> 
        -- do nothing when paused
        return gstate

    IsFinished 
      ->
        do
          -- only if score > highscore
          addToHighscores score
          return gstate { playstate = IsEnded }
          
    IsEnded
      -> 
        do 
          return gstate
  where
    basicFuncs gstate = 
                explodedAsteroidsAnimation $ filterExplodedAsteroids $ filterBulletsOutOfMap $ filterAsteroidsOutOfMap $ moveAsteroids 
                $ moveBullets $ moveUfos $ updateUfoVel $ movePlayer $ bulletCollisionWithAsteroid $ playerCollisionWithAsteroid 
                $ bulletCollisionWithUfo $ moveUfoBullets $ playerCollisionWithUfoBullet $ playerCollisionWithUfo
                $ updateElapsedTime secs gstate


updateElapsedTime :: Float -> GameState -> GameState
updateElapsedTime secs gstate@GameState{elapsedTime = elapsedTime, spawnTimerAst = spawnTimerAst, spawnTimerUfo = spawnTimerUfo, shootTimerUfo = shootTimerUfo} = 
  gstate { elapsedTime = elapsedTime + secs
         , spawnTimerAst = spawnTimerAst + secs
         , spawnTimerUfo = spawnTimerUfo + secs
         , shootTimerUfo = shootTimerUfo + secs }

addToHighscores :: Int -> IO ()
addToHighscores score = do
  fileContent <- readFile "src/highscores.txt"
  let highscore = read fileContent :: Int
  if (score > highscore) 
    then writeFile "src/highscores.txt" (show score)
    else return ()

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) Down  _ _) gstate@(GameState player@(SpaceShip _ _ _ _ (isLeft, isRight, isForward) _) _ _ ufos score _ _ _ _ playstate _)
  = 
    case c of
    'w' -> gstate { 
            player = player {
              direction = (isLeft, isRight, True)
           }}
    'a' -> gstate { 
            player = player {
              direction = (True, isRight, isForward)
           }}
    'd' -> gstate { 
            player = player {
              direction = (isLeft, True, isForward)
           }}
    'q' -> gstate {
            score = score + 10 -- secret score increaser
           }
    'p' | playstate == IsPlaying 
        -> gstate {
            playstate = IsPaused
           }
        | playstate == IsPaused
        -> gstate {
            playstate = IsPlaying
           }
        | otherwise
        -> gstate
    'f' -> gstate {
            playstate = IsFinished -- secret game finisher
           }
    'c' | playstate == IsEnded
        ->
          initialState
        | otherwise
        -> gstate
    _   -> gstate

inputKey (EventKey (Char c) Up  _ _) gstate@(GameState player@(SpaceShip _ _ _ _ (isLeft, isRight, isForward) _) _ _ _ _ _ _ _ _ _ _)
  = 
    case c of
    'w' -> gstate { 
            player = player {
              direction = (isLeft, isRight, False)
           }}
    'a' -> gstate { 
            player = player {
              direction = (False, isRight, isForward)
           }}
    'd' -> gstate { 
            player = player {
              direction = (isLeft, False, isForward)
           }}
    _   -> gstate

inputKey (EventKey (SpecialKey key) Down _ _) gstate@(GameState player@(SpaceShip _ pPos pVel _ _ bullets) _ _ _ _ _ _ _ _ _ _)
  | playstate gstate == IsPlaying
  = 
    case key of
      KeySpace -> gstate { 
            player = player {
              bullets = addBullet 8 pPos pVel bullets
           }}
      _   -> gstate
  | otherwise
  = 
    gstate

inputKey (EventKey (MouseButton btn) Down _ _) gstate@(GameState player@(SpaceShip _ pPos pVel _ _ bullets) _ _ _ _ _ _ _ _ _ _)
  | playstate gstate == IsPlaying
  = 
    case btn of
      LeftButton -> gstate { 
            player = player {
              bullets = addBullet 8 pPos pVel bullets
           }}
      _   -> gstate
  | otherwise
  = 
    gstate

inputKey _ gstate = gstate
