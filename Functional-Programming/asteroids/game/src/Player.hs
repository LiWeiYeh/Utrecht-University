-- This module contains player logic
module Player where

import Model
import Utilities

movePlayer :: GameState -> GameState
movePlayer gstate@(GameState player'@(SpaceShip rotation pPos@(pPosX, pPosY) pVel@(pVelX, pVelY) pRadius (isLeft, isRight, isForward) _) _ _ _ _ _ _ _ _ _ _)
  = moveForward $ rotateLeft $ rotateRight gstate
  where
    rotateLeft :: GameState -> GameState
    rotateLeft gstate
      | isLeft
      = gstate { 
          player = player' {
            rotation = rotation - 2,
            playerVelocity = updateVelocityLeft rotation pVel
        }}
      | otherwise 
      = gstate

    rotateRight :: GameState -> GameState
    rotateRight gstate
      | isRight
      = gstate { 
          player = player' {
            rotation = rotation + 2,
            playerVelocity = updateVelocityRight rotation pVel
        }}
      | otherwise
      = gstate

    moveForward :: GameState -> GameState
    moveForward gstate
      | isForward
      = gstate { 
          player = (player gstate) {
            playerPosition = (pPosX + pVelX * 3, pPosY + pVelY * 3)
        }}
      | otherwise 
      = gstate

    -- updateVelocity based on rotation
    updateVelocityRight :: Rotation -> Velocity -> Velocity
    updateVelocityRight rotation vel@(velX, velY) | moddedRotation rotation >= 0 && moddedRotation rotation <= 90 
                                                 = (velX + (1/45)  , velY - (1/45))
                                                 | moddedRotation rotation > 90 && moddedRotation rotation <= 180 
                                                 = (velX - (1/45)  , velY - (1/45))
                                                 | moddedRotation rotation > 180 && moddedRotation rotation <= 270 
                                                 = (velX - (1/45)  , velY + (1/45))
                                                 | moddedRotation rotation > 270 && moddedRotation rotation <= 360
                                                 = (velX + (1/45)  , velY + (1/45))
                                                 | otherwise
                                                 = vel

    updateVelocityLeft :: Rotation -> Velocity -> Velocity
    updateVelocityLeft rotation vel@(velX, velY) | moddedRotation rotation >= 0 && moddedRotation rotation <= 90 
                                                 = (velX - (1/45)  , velY + (1/45))
                                                 | moddedRotation rotation > 90 && moddedRotation rotation <= 180 
                                                 = (velX + (1/45)  , velY + (1/45))
                                                 | moddedRotation rotation > 180 && moddedRotation rotation <= 270 
                                                 = (velX + (1/45)  , velY - (1/45))
                                                 | moddedRotation rotation > 270 && moddedRotation rotation <= 360
                                                 = (velX - (1/45)  , velY - (1/45))
                                                 | otherwise
                                                 = vel

    -- Helper functions for the rotation of the player. This is important for the updateVelocity functions.
    moddedRotation :: Rotation -> Int
    moddedRotation rotation = (round $ increaseTillBetween0And360 rotation) `mod` 360

    increaseTillBetween0And360 :: Float -> Float
    increaseTillBetween0And360 rotation | rotation < 0 = increaseTillBetween0And360 (rotation + 360)
                                        | rotation > 360 = increaseTillBetween0And360 (rotation - 360)
                                        | otherwise = rotation

-- if player collides, game is done
playerCollisionWithAsteroid :: GameState -> GameState
playerCollisionWithAsteroid gstate@(GameState player asteroids _ _ _ _ _ _ _ _ _)
  | any (\asteroid -> isPlayerWithAsteroidCollision player asteroid) asteroids
  =
    gstate {
      playstate = IsFinished
    }
  | otherwise 
  = 
    gstate
    where
      isPlayerWithAsteroidCollision :: SpaceShip -> Asteroid -> Bool
      isPlayerWithAsteroidCollision (SpaceShip _ posPlayer _ radius1 _ _) (MkAsteroid posAsteroid _ radius2) =
        pythagoreanTheorem posPlayer posAsteroid - radius1 - radius2 <= 0

playerCollisionWithUfo :: GameState -> GameState
playerCollisionWithUfo gstate@(GameState player _ _ ufos _ _ _ _ _ _ _)
  | any (\ufo -> isPlayerWithUfoCollision player ufo) ufos
  =
    gstate {
      playstate = IsFinished
    }
  | otherwise 
  = 
    gstate
    where
      isPlayerWithUfoCollision :: SpaceShip -> UFO -> Bool
      isPlayerWithUfoCollision (SpaceShip _ posPlayer _ radius1 _ _) (MkUFO posAsteroid _ radius2 _) =
        pythagoreanTheorem posPlayer posAsteroid - radius1 - radius2 <= 0

playerCollisionWithUfoBullet :: GameState -> GameState
playerCollisionWithUfoBullet gstate@(GameState player _ _ ufos _ _ _ _ _ _ _)
  | any (\ufo -> isPlayerWithUfoBulletCollision player ufo) ufos
  =
    gstate {
      playstate = IsFinished
    }
  | otherwise 
  = 
    gstate
    where
      isPlayerWithUfoBulletCollision :: SpaceShip -> UFO -> Bool
      isPlayerWithUfoBulletCollision (SpaceShip _ posPlayer _ radius1 _ _) (MkUFO _ _ _ bullets) =
        any (\bullet -> isPlayerWithUfoBulletCollision' player bullet) bullets

      isPlayerWithUfoBulletCollision' :: SpaceShip -> Bullet -> Bool
      isPlayerWithUfoBulletCollision' (SpaceShip _ posPlayer _ radius1 _ _) (MkBullet posBullet _ radius2 ) =
        pythagoreanTheorem posPlayer posBullet - radius1 - radius2 <= 0