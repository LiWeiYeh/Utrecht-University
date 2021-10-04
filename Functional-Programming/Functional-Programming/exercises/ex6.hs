module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"


-- Natural numbers

data Nat = Zero
         | Succ Nat

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

fromInt :: Int -> Nat
fromInt n = case n `compare` 0 of
            LT -> error "negative nr"
            EQ -> Zero
            GT -> Succ (fromInt (n-1))



-- <
lessthan  :: Ord a => [a] -> [a] -> Bool
lessthan [] []     = False
lessthan [x] [y]   = x < y
lessthan [] _      = True
lessthan (_:_) []  = False
lessthan (x:xs) (y:ys) = (x < y) && (lessthan xs ys)

data Complex = C Float Float

instance Num Complex where
    (C a b) + (C x y) = C (a+x) (b+y)
    (C a b) - (C x y) = C (a-x) (b-y)
    (C a b) * (C x y) = C (a*x-b*y) (a*y+b*x)
    negate (C a b) = C (negate a) (negate b)
    abs (C a b) = C (a*a+b*b) 0
    fromInteger i = C (fromInteger i) 0

data StandardSize = Small
                  | Medium
                  | Large

data BikeSize = Standardized StandardSize
              | CustomSize Int

data FrontChainRings = SingleChainRing
                     | DoubleChainRing

data Fender = Plastic
            | Metal

data Gears = Gears FrontChainRings Int

data Bike = CityBike (Maybe Fender) (Maybe Fender)
          | RoadBike Gears
          | MountainBike Gears

data Bikeshop = Bikeshop [Bike]

getFenders :: Bike -> (Maybe Fender, Maybe Fender)
getFenders (CityBike a b) = (a, b)
getFenders (RoadBike _) = (Nothing, Nothing)
getFenders (MountainBike _) = (Nothing, Nothing)

-- byGears :: Bikeshop -> Bikeshop
-- byGears (Bikeshop (x:xs)) = undefined
-- byGears (Bikeshop ((CityBike _ _):xs)) byGears

-- Set a
-- data Set = Set [a]

-- subset :: Eq a => Set a -> Set a -> Bool
-- subset (Set []) _ = Set []
-- subset _ (Set []) = Set []
-- subset (Set (x:xs)) (Set ys) | isInList x ys = Set (x ++ subset (Set xs) (Set ys))
--                                 where
--                                     isInList n [] = False
--                                     isInList n (z:zs) | n == z = True
--                                                       | otherwise = isInList n zs

-- -- Finite
-- class Finite a where
--     elements :: [a]

-- instance Finite Bool where
--     elements = [False, True]

-- instance Finite Char where
--     elements = 

-- data Prop = Basic Bool | Var Char
--           | Not Prop
--           | Prop :/\: Prop | Prop :\/: Prop | Prop :=>: Prop

-- printProp :: Prop -> String
