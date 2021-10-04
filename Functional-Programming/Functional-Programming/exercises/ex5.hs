module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

add1tofloat :: Float -> Float
add1tofloat x = 1.0 + x

-- (Float -> Float) -> Float
justReturnTheFloat :: (Float -> Float) -> Float
justReturnTheFloat add2 = add2 3 + 2

-- Float -> (Float -> Float)
floatToFloatFloat :: Float -> (Float -> Float)
floatToFloatFloat somefloat = theFunc 
                                where theFunc x = somefloat + x

-- (Float -> Float) -> (Float -> Float)
floatFloatToFloatFloat :: (Float -> Float) -> (Float -> Float)
floatFloatToFloatFloat add2 = someOtherFunc
                                    where someOtherFunc x = add2 x

filterr p = concat . map box
                where box x | p x = [x]
                            | otherwise = []

before f g x= g(f x)


