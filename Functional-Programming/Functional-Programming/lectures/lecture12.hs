module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

class Arbitrary a where
    arbitrary :: Gen a

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency listOfGens
        where
            listOfGens :: [(Int, Gen (Tree a))]
            listOfGens = [ (1, genLeaf)
                         , (2, genNode)
                         ]

            genLeaf :: Gen (Tree a)
            genLeaf = return Leaf

            genNode :: Gen (Tree a)
            genNode = do
                (x :: a) <- (arbitrary :: Gen a)
                l <- arbitrary 
                r <- arbitrary
                return $ Node l x r
            

