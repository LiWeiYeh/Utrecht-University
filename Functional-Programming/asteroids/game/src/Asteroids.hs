-- This module contains logic for asteroids or ufos
module Asteroids where

import Model
import Utilities
import System.Random

-- Gets a random asteroid position
getAsteroidPos :: Float -> IO Position
getAsteroidPos margin = do
  a <- randomRIO (1, 4) :: IO Int
  case a of
    1 ->
      do 
        let posX = fromIntegral width / 2 + margin
        posY <- getRandomPos' height
        return (posX, posY)
    2 ->
      do 
        let posX = negate $ fromIntegral width / 2 + margin
        posY <- getRandomPos' height
        return (posX, posY)
    3 ->
      do 
        let posY = fromIntegral height / 2 + margin
        posX <- getRandomPos' width
        return (posX, posY)
    _ ->
      do 
        let posY = negate $ fromIntegral height / 2 + margin
        posX <- getRandomPos' width
        return (posX, posY)
  where
    width = 1000 :: Int
    height = 800 :: Int

    getRandomPos' :: Int -> IO Float
    getRandomPos' range = do
      a <- randomRIO (0, range) :: IO Int
      let pos = (fromIntegral a - (fromIntegral range / 2))
      return pos

-- Gets a random velocity based on it's own position and the player's position
getAsteroidVel :: Position -> Position -> Float -> IO Velocity
getAsteroidVel posAsteroid posPlayer secs = do
  marginX <- randomRIO (negate maxMargin, maxMargin) :: IO Float
  marginY <- randomRIO (negate maxMargin, maxMargin) :: IO Float

  -- making speed faster over time
  let speedBoost = log secs
  let (x1, y1) = getAsteroidVel' posAsteroid posPlayer marginX marginY speedBoost
  let (x2, y2) = (x1 / distance, y1 / distance)

  return (x2, y2)

  where
    maxMargin = 200
    getAsteroidVel' :: Position -> Position -> Float -> Float -> Float -> Velocity
    getAsteroidVel' (posAsteroidX, posAsteroidY) (posPlayerX, posPlayerY) x y s =
      (s * (posPlayerX - posAsteroidX + x), s * (posPlayerY - posAsteroidY + y))

    distance :: Float
    distance = abs $ pythagoreanTheorem posAsteroid posPlayer

addAsteroid :: Position -> Velocity -> Float -> [Asteroid] -> [Asteroid]
addAsteroid pos velo radius asteroids = MkAsteroid pos velo radius : asteroids

moveAsteroids :: GameState -> GameState
moveAsteroids gstate = 
  gstate { asteroids = moveAsteroids (asteroids gstate) }
  where
    moveAsteroids :: [Asteroid] -> [Asteroid]
    moveAsteroids = map (\(MkAsteroid (posX, posY) (velX, velY) radius) -> MkAsteroid (posX + velX, posY + velY) (velX, velY) radius)

filterAsteroidsOutOfMap :: GameState -> GameState
filterAsteroidsOutOfMap gstate = 
  gstate {
    asteroids = filterAsteroids' (asteroids gstate)
  }
  where
    filterAsteroids' :: [Asteroid] -> [Asteroid]
    filterAsteroids' = filter (\(MkAsteroid (posX, posY) _ radius) -> 
                                posX <= ((1000 / 2) + radius) && 
                                posX >= ((negate $ 1000 / 2) - radius) &&
                                posY <= ((800 / 2) + radius) &&
                                posY >= ((negate $ 800 / 2) - radius))

explodedAsteroidsAnimation :: GameState -> GameState
explodedAsteroidsAnimation gstate =
  gstate {
    explodedAsteroids = explodedAsteroidsAnimation' (explodedAsteroids gstate)
  }
  where
    explodedAsteroidsAnimation' :: [ExplodedAsteroid] -> [ExplodedAsteroid]
    explodedAsteroidsAnimation' = map (\(MkExplodedAsteroid (posX, posY) rotation size)
                                        -> MkExplodedAsteroid (posX, posY) (rotation + 10) (size - 1.5))

filterExplodedAsteroids :: GameState -> GameState
filterExplodedAsteroids gstate =
  gstate {
    explodedAsteroids = filterExplodedAsteroids' (explodedAsteroids gstate)
  }
  where
    filterExplodedAsteroids' :: [ExplodedAsteroid] -> [ExplodedAsteroid]
    filterExplodedAsteroids' = filter (\(MkExplodedAsteroid _ _ size) -> size > 0)










addUfo :: Position -> Velocity -> Float -> [UFO] -> [UFO]
addUfo pos velo radius ufos = MkUFO pos velo radius [] : ufos

moveUfos :: GameState -> GameState
moveUfos gstate = 
  gstate { ufos = moveUfos' (ufos gstate) }
  where
    moveUfos' :: [UFO] -> [UFO]
    moveUfos' = map (\(MkUFO (posX, posY) (velX, velY) radius bullets) -> MkUFO (posX + velX, posY + velY) (velX, velY) radius bullets)

updateUfoVel :: GameState  -> GameState 
updateUfoVel gstate = 
  gstate { ufos = map updateUfoVel' (ufos gstate) }
  where
    updateUfoVel' :: UFO -> UFO
    updateUfoVel' ufo@(MkUFO (posX, posY) (velX, velY) r bullets) | posX <= (-550) = MkUFO (posX, posY) (negate velX, velY) r bullets
                                                                  | posX >= 550 = MkUFO (posX, posY) (negate velX, velY) r bullets
                                                                  | posY >= 0 && posY >= 400 = MkUFO (posX, posY) (velX, negate velY) r bullets
                                                                  | posY >= 0 && posY <= 200 = MkUFO (posX, posY) (velX, negate velY) r bullets
                                                                  | posY <= 0 && posY <= negate 400 = MkUFO (posX, posY) (velX, negate velY) r bullets
                                                                  | posY <= 0 && posY >= negate 200 = MkUFO (posX, posY) (velX, negate velY) r bullets
                                                                  | otherwise = ufo

getUfoPos :: IO Position
getUfoPos = do
  a <- randomRIO (1, 4) :: IO Int
  case a of
    1 ->
      do 
        margin <- getRandomMargin'
        return (negate 549, negate 400 + margin)
    2 ->
      do 
        margin <- getRandomMargin'
        return (549, 400 - margin)
    3 ->
      do 
        margin <- getRandomMargin'
        return (negate 549, negate 400 + margin)
    _ ->
      do 
        margin <- getRandomMargin'
        return (549, 400 - margin)
  where
    getRandomMargin' :: IO Float
    getRandomMargin' = do
      margin <- randomRIO (0, 120) :: IO Int
      return $ fromIntegral margin

