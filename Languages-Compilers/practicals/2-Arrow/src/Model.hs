module Model where

-- Exercise 1
data Token = TArrow
           | TDot
           | TComma
           | TGo
           | TTake
           | TMark
           | TNothing
           | TTurn
           | TCase
           | TOf
           | TEnd
           | TLeft
           | TRight
           | TFront
           | TSemicolon
           | TEmpty
           | TLambda
           | TDebris
           | TAsteroid
           | TBoundary
           | TUnderscore
           | TIdent Ident
           deriving (Eq, Show)

type Ident = String

-- Exercise 2
data Program = Program [Rule]
             deriving (Show, Eq)

data Rule = Rule Model.Ident [Cmd]
          deriving (Show, Eq)

data Cmd = Go | Take | Mark | Nothing'
         | Turn Direction
         | Case Direction [Alt]
         | Ident Ident
         deriving (Show, Eq)

data Alt = Alt Pat [Cmd]
         deriving (Show, Eq)

data Pat = Empty
         | Lambda
         | Debris
         | Asteroid
         | Boundary
         | Underscore
         deriving (Show, Eq)

data Direction = Left
               | Right
               | Front
               deriving (Show, Eq)
