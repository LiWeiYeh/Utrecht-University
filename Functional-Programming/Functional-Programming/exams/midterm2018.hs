module Main where

import Lib

import Data.Foldable
import Data.List

main :: IO ()
main = putStrLn "hello world"


-- data Category = Start | MainCourse | Dessert | Snack
-- data Ingredient = Ingredient String String
-- data Ingredients = Ingredients [Ingredient]
-- data Task = Task String String Int
-- data Tasks = Tasks [Task]
-- data Recipe = Recipe Category Ingredients Tasks

-- cookingTime (Recipe _ _ []) = 0
-- cookingTime (Recipe _ _ [Task _ _ x]) = x
-- cookingTime (Recipe _ _ ((Task _ _ x):ys)) = x + cookingTime (Recipe Category _ ys)

-- foodsWithout :: [String] -> [Recipe] -> [Recipe]
-- foodsWithout [] rs = rs
-- foodsWithout _ _ = []
-- foodsWithout is rs = filter p rs
--                         where
--                             p (Recipe _ ingredients _) = not (elem (\Ingredient nm _ -> elem nm ingredients))


data PSTree a = Leaf Integer | Node Integer a (PSTree a) (PSTree a)

tr = Node 2 'd' (Node 1 'z' (Leaf 0) (Leaf 0))(Leaf 0)

pre :: PSTree a -> Integer 
pre (Leaf p) = p
pre (Node p _ _ _) = p


mapPS :: (a -> b) -> PSTree a -> PSTree b
mapPS func (Leaf a) = Leaf a
mapPS func (Node x a lt rt) = (Node x (func a) (mapPS func lt) (mapPS func rt))



