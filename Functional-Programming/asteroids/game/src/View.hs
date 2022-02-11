{-# LANGUAGE TupleSections #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gstate = do
  -- get highest score
  s <- readFile "src/highscores.txt" :: IO String
  return $ viewPure s gstate

viewPure :: String -> GameState -> Picture
viewPure = drawing

drawing :: String -> GameState -> Picture
drawing highscore gstate@(GameState player@(SpaceShip pRotation pPos pVel pRadius _ bullets) asteroids explodedAsteroids ufos score elapsedTime _ _ _ playstate _) = pictures 
  ( bulletPictures
    ++
    ufoBullets
    ++
    asteroidPictures
    ++
    explodedAsteroidPictures
    ++
    ufoPictures
    ++
    [ color playerColour $ uncurry translate pPos $ Rotate pRotation $ rectangleSolid 25 40     -- player 
    , color playerForceFieldColour $ uncurry translate pPos $ circle pRadius                    -- player forcefield
    , color green $ translate (-500) 350 $ scale 0.3 0.3 $ Text $ scoreText score               -- the score
    , color green $ translate (-500) 250 $ scale 0.3 0.3 $ Text ("Highscore: " ++ highscore)    -- the highscore
    , color green $ translate (-500) 300 $ scale 0.3 0.3 $ Text $ elapsedTimeText elapsedTime   -- the elapsed time
    , showState $ playstate                                                                     -- the state
    ]
  )
  where
    playerColour = light (light blue) 
    playerForceFieldColour = blue
    bulletColour = cyan
    asteroidColour = red
    ufoColour = light (light red)
    explodedAsteroidColour = dark (dark red)

    scoreText :: Int -> String
    scoreText score = "Score: " ++ show score

    -- round to 2 decimals
    elapsedTimeText :: Float -> String
    elapsedTimeText time = "Time: " ++ show (roundToDecimals time 2)
      where
        roundToDecimals :: Float -> Int -> Float
        roundToDecimals x n = (fromIntegral (floor (x * t))) / t
          where
            t = 10^n

    bulletPictures :: [Picture]
    bulletPictures = foldr f [] bullets
    f (MkBullet pos _ radius) r = (uncurry translate pos $ color bulletColour $ circle radius) : r

    asteroidPictures :: [Picture]
    asteroidPictures = foldr g [] asteroids
    g (MkAsteroid pos _ radius) r = (uncurry translate pos $ color asteroidColour $ circleSolid radius) : r

    explodedAsteroidPictures :: [Picture]
    explodedAsteroidPictures = foldr h [] explodedAsteroids
    h (MkExplodedAsteroid pos rotation size) r = (uncurry translate pos $ color explodedAsteroidColour $ Rotate rotation $ rectangleSolid size size) : r

    ufoPictures :: [Picture]
    ufoPictures = foldr i [] ufos
    i (MkUFO pos _ radius _) r = (uncurry translate pos $ color ufoColour $ circle radius) : r

    ufoBullets :: [Picture]
    ufoBullets = foldr j [] ufobullets
      where
        ufobullets = concatMap (\(MkUFO _ _ _ bullets) -> bullets) ufos
        j (MkBullet pos _ radius) r = (uncurry translate pos $ color (dark red) $ circle radius) : r

    showState :: PlayState -> Picture
    showState playstate | playstate == IsPaused  = color green $ translate (-60)  0 $ scale 0.3 0.3 $ Text "Paused"
                        | playstate == IsEnded   = color green $ translate (-300) 0 $ scale 0.3 0.3 $ Text "You died press 'c' to play again"
                        | otherwise              = Blank



initialDisplay :: Display
initialDisplay = InWindow name size pos
  where
    name = "Asteroids Game"
    size = (width, height)
      where
        width = 1000
        height = 800
    pos  = (460, 140)
