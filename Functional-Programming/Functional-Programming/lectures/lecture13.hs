module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"

evalOp :: ArithOp -> Float -> Float -> Float
evalOp Plus = (+)


evalInstr' :: Instr -> Stack -> Stack
evalInstr' (Number f) fs = f : fs
evalInstr' (Operation o) (f1 : f2 : fs) = evalOp o f1 f2 : fs


getState :: State a a
getState = S (\a -> (a, a))

increment :: State Int ()
increment = S (\i -> ((), i + 1))

label' :: Tree a -> State Int (Tree (Int, a))
label' Leaf = return Leaf
-- label' (Node l v r) = do
--     l' <- 
