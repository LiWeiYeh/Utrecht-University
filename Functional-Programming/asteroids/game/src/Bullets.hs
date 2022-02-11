-- This module contains bullet logic
module Bullets where

import Model
import Utilities

addBullet :: Float -> Position -> Velocity -> [Bullet] -> [Bullet]
addBullet speed (posX, posY) (velX, velY) bullets =
  ((MkBullet 
      (posX + velX * 40, posY + velY * 40) 
      (velX * speed, velY * speed)
      2)
    : bullets)

moveBullets :: GameState -> GameState
moveBullets gstate@(GameState { player = player }) = 
  gstate {
    player = player {
      bullets = moveBullets' (bullets player) 
    }
  }
  where
    moveBullets' :: [Bullet] -> [Bullet]
    moveBullets' = map (\(MkBullet (posX, posY) (velX, velY) radius) -> MkBullet (posX + velX, posY + velY) (velX, velY) radius)
 

filterBulletsOutOfMap :: GameState -> GameState
filterBulletsOutOfMap gstate@(GameState { player = player, ufos = ufos}) = 
  gstate {
    player = player {
      bullets = filterBullets' (bullets player)
    },
    ufos = filterUfoBullets ufos
  }
  where
    filterUfoBullets :: [UFO] -> [UFO]
    filterUfoBullets = map (\(MkUFO pos vel r bullets) -> MkUFO pos vel r (filterBullets' bullets))

    filterBullets' :: [Bullet] -> [Bullet]
    filterBullets' = filter (\(MkBullet (posX, posY) _ radius) -> 
                                posX <= ((1000 / 2) + radius) && 
                                posX >= ((negate $ 1000 / 2) - radius) &&
                                posY <= ((800 / 2) + radius) &&
                                posY >= ((negate $ 800 / 2) - radius))

-- if bullet is collided with asteroid, remove collided bullet and asteroid. The asteroid will be added as an "explodingAsteroid"
bulletCollisionWithAsteroid :: GameState -> GameState
bulletCollisionWithAsteroid gstate@(GameState player@(SpaceShip _ _ _ _ _ bullets) asteroids explodedAsteroids _ score _ _ _ _ _ _)
  | checkCollisionsAsteroidsBullets bullets asteroids
  = 
    gstate {
      asteroids = filterAsteroidsCollision bullets asteroids,
      player = player {
        bullets = filterBulletsCollision asteroids bullets
      },
      explodedAsteroids = addExplodedAsteroids bullets asteroids explodedAsteroids,
      score = score + 10
    }
  | otherwise 
  =
    gstate

  where
    addExplodedAsteroids :: [Bullet] -> [Asteroid] -> [ExplodedAsteroid] -> [ExplodedAsteroid]
    addExplodedAsteroids bullets asteroids explodedAsteroids = 
      explodedAsteroids ++ (foldr (\(MkAsteroid (posX, posY) _ radius) r -> MkExplodedAsteroid (posX, posY) 0 radius : r) [] getKilledAsteroids)
      where
        getKilledAsteroids :: [Asteroid]
        getKilledAsteroids = filter (\asteroid -> (isAsteroidCollisionWithAnyBullets asteroid bullets)) asteroids

    filterAsteroidsCollision :: [Bullet] -> [Asteroid] -> [Asteroid]
    filterAsteroidsCollision bullets =
      filter (\asteroid -> not (isAsteroidCollisionWithAnyBullets asteroid bullets))

    filterBulletsCollision :: [Asteroid] -> [Bullet] -> [Bullet]
    filterBulletsCollision asteroids =
      filter (\bullet -> not (isBulletCollisionWithAnyAsteroids bullet asteroids))

    checkCollisionsAsteroidsBullets :: [Bullet] -> [Asteroid] -> Bool
    checkCollisionsAsteroidsBullets bullets asteroids = any (\b -> isBulletCollisionWithAnyAsteroids b asteroids) bullets

    isAsteroidCollisionWithAnyBullets :: Asteroid -> [Bullet] -> Bool
    isAsteroidCollisionWithAnyBullets asteroid' = any (\b -> isBulletWithAsteroidCollision b asteroid')

    isBulletCollisionWithAnyAsteroids :: Bullet -> [Asteroid] -> Bool
    isBulletCollisionWithAnyAsteroids bullet' = any (isBulletWithAsteroidCollision bullet')

    isBulletWithAsteroidCollision :: Bullet -> Asteroid -> Bool
    isBulletWithAsteroidCollision (MkBullet posBullet _ radius1) (MkAsteroid posAsteroid _ radius2) =
      pythagoreanTheorem posBullet posAsteroid - radius1 - radius2 <= 0

-- if bullet is collided with asteroid, remove collided bullet and asteroid. The asteroid will be added as an "explodingAsteroid"
bulletCollisionWithUfo :: GameState -> GameState
bulletCollisionWithUfo gstate@(GameState player@(SpaceShip _ _ _ _ _ bullets) _ explodedAsteroids ufos score _ _ _ _ _ _)
  | checkCollisionsUfosBullets bullets ufos
  = 
    gstate {
      ufos = filterUfosCollision bullets ufos,
      player = player {
        bullets = filterBulletsCollision ufos bullets
      },
      explodedAsteroids = addExplodedUfos bullets ufos explodedAsteroids,
      score = score + 30
    }
  | otherwise 
  =
    gstate

  where
    addExplodedUfos :: [Bullet] -> [UFO] -> [ExplodedAsteroid] -> [ExplodedAsteroid]
    addExplodedUfos bullets ufos explodedUfos = 
      explodedUfos ++ (foldr (\(MkUFO (posX, posY) _ radius _) r -> MkExplodedAsteroid (posX, posY) 0 radius : r) [] getKilledUfos)
      where
        getKilledUfos :: [UFO]
        getKilledUfos = filter (\ufo -> (isUfoCollisionWithAnyBullets ufo bullets)) ufos

    filterUfosCollision :: [Bullet] -> [UFO] -> [UFO]
    filterUfosCollision bullets =
      filter (\ufo -> not (isUfoCollisionWithAnyBullets ufo bullets))

    filterBulletsCollision :: [UFO] -> [Bullet] -> [Bullet]
    filterBulletsCollision ufos =
      filter (\bullet -> not (isBulletCollisionWithAnyUfos bullet ufos))

    checkCollisionsUfosBullets :: [Bullet] -> [UFO] -> Bool
    checkCollisionsUfosBullets bullets ufos = any (\b -> isBulletCollisionWithAnyUfos b ufos) bullets

    isUfoCollisionWithAnyBullets :: UFO -> [Bullet] -> Bool
    isUfoCollisionWithAnyBullets ufo' = any (\b -> isBulletWithAsteroidCollision b ufo')

    isBulletCollisionWithAnyUfos :: Bullet -> [UFO] -> Bool
    isBulletCollisionWithAnyUfos bullet' = any (isBulletWithAsteroidCollision bullet')

    isBulletWithAsteroidCollision :: Bullet -> UFO -> Bool
    isBulletWithAsteroidCollision (MkBullet posBullet _ radius1) (MkUFO posAsteroid _ radius2 _) =
      pythagoreanTheorem posBullet posAsteroid - radius1 - radius2 <= 0

ufosShoot :: GameState -> GameState 
ufosShoot gstate@(GameState player@(SpaceShip _ pos1@(posX1, posY1) _ _ _ bullets) _ _ ufos _ _ _ _ _ _ _) = 
  gstate {
    ufos = map ufoShoot' ufos
  }  
  where
    ufoShoot' :: UFO -> UFO
    -- shoots at player position
    ufoShoot' ufo@(MkUFO pos2@(posX2, posY2) vel@(velX, velY) r bullets) = MkUFO pos2 vel r (addBullet 2 (posX2, posY2) ((posX1 - posX2) / distance pos2, (posY1 - posY2) / distance pos2) bullets)

    distance :: Position -> Float
    distance pos2 = abs $ pythagoreanTheorem pos2 pos1

moveUfoBullets :: GameState -> GameState 
moveUfoBullets gstate@(GameState player@(SpaceShip _ pos1@(posX1, posY1) _ _ _ bullets) _ _ ufos _ _ _ _ _ _ _) = 
  gstate {
    ufos = map (\(MkUFO pos vel r bullets) ->  MkUFO pos vel r $ moveBullets' bullets) ufos
  }
  where
    moveBullets' :: [Bullet] -> [Bullet]
    moveBullets' = map (\(MkBullet (posX, posY) (velX, velY) radius) -> MkBullet (posX + velX, posY + velY) (velX, velY) radius)
