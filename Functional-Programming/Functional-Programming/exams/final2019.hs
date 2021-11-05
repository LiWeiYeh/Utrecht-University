data First a = TheFirst a 
             | Never 
             deriving (Show, Eq, Ord)

-- ex 1a
instance Functor First where
    fmap f (TheFirst a) = TheFirst (f a)
    fmap f Never = Never

class Monoid m where
    mempty :: m
    (<>)   :: m -> m -> m



-- mempty is the identity function
-- ex 1b
instance Monoid (First a) where
    mempty = Never
    (<>) Never x = x
    (<>) x@(TheFirst _) _ = x 


data These l r = Neither
               | This l
               | That r
               | Both l r

-- ex 1c
-- perhaps just pattern match without use of functor?
instance Functor These where
    fmap' f g Neither    = Neither
    fmap' f g (This l)   = This (f l)
    fmap' f g (That r)   = That (g r)
    fmap' f g (Both l r) = Both ((f l) (g r))

bimapThese :: (l -> l') -> (r -> r') -> These l r -> These l' r'
bimapThese _ _ Neither = Neither
bimapThese f _ (This l) = This (f l)
bimapThese _ g (That r) = That (g r)
bimapThese f g (Both l r) = Both (f l) (g r)


-- ex 1d
-- ???
instance Bifunctor These where
    bimap = bimapThese

instance Bifunctor (,) where
    bimap = bimapPair

class Bifunctor t where
    bimap :: (a -> a') -> (b -> b') -> t a b -> t a' b'

foo :: Bifunctor t => t Int Float -> t String String
foo = bimap show show



-- ex 2a
isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- isPrefixOf (x:xs) (y:ys) | xs == [] = x == y
--                          | x == y = isPrefixOf xs ys
--                          | otherwise = False
isPrefixOf [] _ = True
isPrefixOf (x:xs) [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

data First a = TheFirst a 
             | Never 
             deriving (Show, Eq, Ord)

-- ex 2b
findFirst :: [a] -> [a] -> First Int


before :: Eq a => [a] -> [a] -> [a] -> Bool
before xs ys zs = case (findFirst xs zs, findFirst ys zs) of
    (Never, _) -> False
    (_, Never) -> False
    (TheFirst i, TheFirst j) -> i <= j

-- ex 2c
concatSpec :: Eq a => ([a] -> [a] -> [a]) -> [a] -> [a] -> Bool
concatSpec concatImpl xs ys = before xs ys (xs `concatImpl` ys)

-- the ys occurence in the before function can be in the middle of the xs occurence, meaning the length of the 
-- zs can be smaller than the length of xs + ys


-- ex 2d
concatSpec :: Eq a => ([a] -> [a] -> [a]) -> [a] -> [a] -> Bool
concatSpec concatImpl xs ys = let zs = xs ‘concatImpl‘ ys
                              in before xs ys zs && (length xs + length ys == length zs)



-- ex 3a
lookup :: Ord k => k -> Map k v -> Maybe v

combineLookup :: Ord k => (v -> v -> Maybe b) -> k -> k -> Map k v -> Maybe b
combineLookup f k1 k2 kv = do
    v1 <- lookup k1 kv
    v2 <- lookup k2 kv

    f v1 v2


f w = do x <- g w
         let xs = do z <- [1, 2]
                     v <- ['a', 'b']
                     return (z, v)
         y <- h (snd (head xs))
         return y

f w = g w >>= \x -> 
        let xs = [1, 2] >>= \z -> ['a','b'] >>= \v -> return (z, v) in h (snd (head xs)) >>= \y -> 
            return y

-- ex 3b
main = do (fp:h:_) <- getArgs
            putStrLn h
            s <- readFile fp
            return (length s)

main = 
    getArgs >>= \(fp:h:_) -> 
        putStrLn h >>= \_ ->
            readFile fp >>= \s ->
                return (length s)


-- ex 3c
-- ???
data Log a = MkLog [String] a

withLogging :: Log a -> IO a
withLogging (MkLog l a) = do 
    mapM_ putStrLn l
    return a

log :: String -> Log ()
log x = MkLog [x] ()

-- ex 3d
readInput :: Log Int
readInput = do 
    log "about to read some input"
    return 5

computeSomething :: Log String
computeSomething = do 
    i <- readInput
    log "read some input"
    let out = i * i
    log "computed something"
    return (show out)

computeIO :: IO String
computeIO = withLogging computeSomething

instance Monad Log where
    return :: a -> Log a
    return x = MkLog [] x
    (>>=) :: Log a -> (a -> Log b) -> Log b
    (>>=) (MkLog ls x) k = let MkLog ls' y = k x
                           in MkLog (ls ++ ls') y

-- ex 3e
withoutLogging :: Log a -> IO a
withoutLogging (MkLog _ a) = return a


-- ex 4a
pf: map f (foldr (\x r -> g x : r) [] xs) = map (f . g) xs 

-- let xs = []

map f (foldr (\x r -> g x : r) [] [])
= def foldr
map f []
= def map
[]
= def map
map (f . g) []

-- let xs = (x:xs)

map f (foldr (\x r -> g x : r) [] (x:xs))
= def foldr
map f ((\x r -> g x : r) x (foldr (\x r -> g x : r) [] (xs)))
= def lambda func
map f (g x : (foldr (\x r -> g x : r) [] (xs)))
= def map
f (g x) : map f (foldr (\x r -> g x : r) [] (xs))
= I.H.
f (g x) : map (f . g) xs
= def (.)
(f.g) x : map (f . g) xs
= def map
map (f . g) (x:xs)


-- ex 4b
pf: size xs = length . toList xs

-- let xs = Leaf
size Leaf
= def Leaf
0
= def length
length []
= def toList
length (toList Leaf)
= def (.)
length . toList Leaf

-- let xs = (Node l x r)
size (Node l x r)
= def size
size l + 1 + size r
= I.H.
length (toList l) ++ 1 ++ length (toList r)
= def +
length (toList l) ++ (1 + 0) ++ length (toList r)
=
length (toList l) ++ (1 + length []) ++ length (toList r)
= def length 
length (toList l) ++ (length (x:[])) ++ length (toList r)
= 
length (toList l) ++ (length [x]) ++ length (toList r)
= lemma j 
length (toList l ++ [x] ++ toList r)
= def toList
length (toList (Node l x r))
= def (.)
length . toList (Node l x r)


-- ex 5a
A   WHNF -> (:) constructor
B   not fully evaluated ->
    False
C   WHNF -> lambda func
D   not fully evaluated
    -> e
E   WHNF -> function applied with too few args

-- ex 5b
C


-- RICHARD