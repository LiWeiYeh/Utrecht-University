module Main where

import Lib
import Data.List

main :: IO ()

main = interact work
    where work = intercalate " / ".map reverse.lines