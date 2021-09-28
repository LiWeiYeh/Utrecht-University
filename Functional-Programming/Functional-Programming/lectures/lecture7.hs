module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

data Prop = Basic Bool
          | Var Char
          | Not Prop
          | Prop :/\: Prop
          | Prop :\/: Prop
          | Prop :=>: Prop
          deriving Show

type Assignment = Map Char Bool


