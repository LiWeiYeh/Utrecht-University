module Main where

import Data.Foldable
import Data.List

main :: IO ()
main = undefined


tuple :: Monad m => m a -> m b -> m (a, b)
tuple ta tb = do
    a <- ta
    b <- tb
    return (a,b)
