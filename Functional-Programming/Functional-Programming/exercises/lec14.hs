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

get :: State s s
get = S (id)

modify :: (s -> s) -> State s ()
modify f = S (\s -> ((), f s))

put :: s -> State s ()
put s = (\_ -> ((), s))

modify' f = do
    s <- get
    put (f s)

put' s = modify (const s)

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f z = foldl'g (return z)
    where
        g r x = do
            a <- r
            f a x

data Error m a = Error m | OK a

instance Functor (Error m) where
    fmap f (Error m) = Error m
    fmap f (Ok a)    = Ok (f a)


instance Monad (Error m) where
    return = Ok
    (>>=) (Error m) _ = m
    (>>=) (Ok a) f = f a







