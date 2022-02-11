-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1


type Lives = Int
type Position = (Float, Float)
type Velocity = (Float, Float)
type Rotation = Float
type IsLeft = Bool
type IsRight = Bool
type IsForward = Bool
type Direction = (IsLeft, IsRight, IsForward)

data Bullet = MkBullet { bulletPosition :: Position
                       , bulletVelocity :: Velocity
                       , bulletRadius   :: Float
                       }

data PlayState = IsPlaying
               | IsPaused
               | IsFinished
               | IsEnded
               deriving (Show, Eq)

data Asteroid = MkAsteroid { asteroidPosition  :: Position
                           , asteroidVelocity  :: Velocity
                           , asteroidRadius    :: Float
                           }

data ExplodedAsteroid = 
    MkExplodedAsteroid { explodedAsteroidPosition :: Position
                       , explodedAsteroidRotation :: Rotation
                       , explodedAsteroidSize     :: Float
                       }

data UFO = MkUFO { ufoPosition :: Position
                 , ufoVelocity :: Velocity
                 , ufoRadius   :: Float
                 , ufoBullets  :: [Bullet] }

data SpaceShip = SpaceShip { rotation          :: Rotation
                           , playerPosition    :: Position
                           , playerVelocity    :: Velocity
                           , playerRadius      :: Float
                           , direction         :: Direction
                           , bullets           :: [Bullet]
                           }

data GameState = GameState { player            :: SpaceShip
                           , asteroids         :: [Asteroid]
                           , explodedAsteroids :: [ExplodedAsteroid]
                           , ufos              :: [UFO]
                           , score             :: Int
                           , elapsedTime       :: Float 
                           , spawnTimerAst     :: Float
                           , spawnTimerUfo     :: Float
                           , shootTimerUfo     :: Float
                           , playstate         :: PlayState
                           , highscore         :: Int
                           }



initialState :: GameState
initialState = GameState (SpaceShip 0 (0,0) (0,1) 25 (False, False, False) []) [] [] [] 0 0.1 0.1 0.1 0.1 IsPlaying 0
