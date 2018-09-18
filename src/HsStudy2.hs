-- |
module HsStudy2
    ( pick
    , toss
    , experiment
    , timesFound
    , remdups
    , countElems
    , fairChance
    , biasChance
    , powerset1
    , powerset2
    , powerset3
    , powerset4
    , evenPair
    , fibStep
    , fibPair
    , fastFib
    , fib
    , combination
    , xseq
    , canonize
    , unique
    , getTriple
    , putLine
    , echoLine
    ) where

import           Control.Applicative
import           Control.Monad

-- You have two coins, labeled Biased  and Fair.  The Biased coin has two
-- Heads and the Fair  coin has one Head and one Tail.  Pick one of these
-- coins at random,  toss it and observe  the result. If the  result is a
-- Head, What is the probability that you picked up a Biased coin?
data CoinType
    = Biased
    | Fair

data Coin
    = Head
    | Tail

instance Show CoinType where
    show Biased = "Biased"
    show Fair   = "Fair"

instance Show Coin where
    show Head = "H"
    show Tail = "T"

instance Eq Coin where
    Head == Head = True
    Tail == Tail = True
    _ == _ = False


instance Eq CoinType where
    Biased == Biased = True
    Fair == Fair     = True
    _ == _           = False

instance Ord CoinType where
    Biased `compare` Biased = EQ
    Biased `compare` Fair   = LT
    Fair `compare` Fair     = EQ
    Fair `compare` Biased   = GT

-- pick a coin; has 2 choices of either Biased or Fair
pick :: [CoinType]
pick = [Biased, Fair]

-- tossing a coin
-- tossing can yield either of the 2 faces of a coin
toss :: CoinType -> [Coin]
toss Fair   = [Head, Tail]
toss Biased = [Head, Head]

-- conduct an experiment with list of Coins
experiment :: [CoinType]
experiment =
    pick >>= \coin -- pick a coin at random
     ->
        toss coin >>= \result -- toss the coin to get a result
         ->
            guard (result == Head) >> -- test the result only for Heads
            return coin -- return the filtered result

-- chance of picking a Biased coin based on the experiment
-- percentage of chance for fair coin over biased coin
fairChance :: (Fractional a, Integral a1) => [(CoinType, a1)] -> a
fairChance []                       = 0.0
fairChance [(Biased, b), (Fair, f)] = fromIntegral f / fromIntegral (b + f)

-- percentage of chance for biased coin over fair coin
biasChance :: (Fractional a, Integral b) => [(CoinType, b)] -> a
biasChance []                       = 0.0
biasChance [(Biased, b), (Fair, f)] = fromIntegral b / fromIntegral (b + f)

-- auxilliary function for finding number of times an element occurs in list
timesFound :: (Ord a, Eq a) => a -> [a] -> Int
timesFound _ [] = 0
timesFound x xs = (length . filter (== x)) xs

-- remove duplicate elements from a list
remdups :: (Eq a) => [a] -> [a]
remdups []     = []
remdups (x:xs) = x : filter (/= x) (remdups xs)

-- count the number of occurrences of each element in a list
countElems :: (Eq a, Ord a) => [a] -> [(a, Int)]
countElems xs = remdups [(c, timesFound c xs) | c <- xs]

-- powerset of a list of elements in 3 different formats
powerset1 :: [a] -> [[a]]
powerset1 = filterM (const [True, False])

powerset2 :: [a] -> [[a]]
powerset2 [] = [[]]
powerset2 (x:xs) = l ++ map (x :) l
  where
    l = powerset2 xs

powerset3 :: [a] -> [[a]]
powerset3 []     = [[]]
powerset3 (x:xs) = powerset3 xs >>= \set -> [set, x : set]

powerset4 :: [a] -> [[a]]
powerset4 = map concat . mapM (\x -> [[], [x]])

{-
λ> powerset3 "abc"
["","a","b","ab","c","ac","bc","abc"]
λ> powerset2 "abc"
["","c","b","bc","a","ac","ab","abc"]
λ> powerset1 "abc"
["abc","ab","ac","a","bc","b","c",""]
-}
-- even pair from lists
evenPair ::
       (Monad m, Alternative m, Integral a, Integral b)
    => m a
    -> m b
    -> m (a, b)
evenPair l m = l >>= \x -> m >>= \y -> guard (even x && even y) >> return (x, y)

-- λ> evenPair [1..5] [5..10]
-- [(2,6),(2,8),(2,10),(4,6),(4,8),(4,10)]

-- Fibonacci numbers
-- fastFib for an efficient implementation of the fibonacci
fibStep :: (Integral a) => (a, a) -> (a, a)
fibStep (x, y) = (y, x + y)

fibPair :: (Integral a) => a -> (a, a)
fibPair n
    | n == 0 = (0, 1)
    | otherwise = fibStep . fibPair $ (n - 1)

fastFib :: (Integral a) => a -> a
fastFib = fst . fibPair

-- a lazy version of the fibonacci
lazyFib :: Int -> Int -> [Int]
lazyFib x y = x : lazyFib y (x + y)

fibseq :: [Int]
fibseq = lazyFib 0 1

fib :: Int -> Int
fib n = fibseq !! n

-- combination nCr
fact :: Integer -> Integer
fact = product . enumFromTo (1 :: Integer)

combination :: Integer -> Integer -> Integer
combination n r = fact n `div` (fact n * fact (n - r))

-- sequencing a list of values
-- re-implement the sequence function
xseq :: (Monad m) => [m a] -> m [a]
xseq = foldr mcons (return [])

mcons :: (Monad m) => m a -> m [a] -> m [a]
mcons x y = do
    a <- x
    b <- y
    return (a : b)

{- λ> xseq [Just 1, Just 2]
 -     Just [1, 2]
 - λ> xseq [[1,2],[3,4]]
 -     [[1, 3], [1, 4], [2, 3], [2, 4]] -}


-- canonize into a list of integers, where each distinct value gets
-- the next highest number, if vals0 = ['d', 'b', 'd', 'd', 'a']
-- then canonize vals0 = [0, 1, 0, 0, 2]
canonize :: (Eq a, Num b, Enum b) => [a] -> [b]
canonize []   = []
canonize [_]  = [0]
canonize vals = map head [[n | (a, n) <- ranks, a == b] | b <- vals]
  where
    ranks = zip (unique vals) [0 ..]

-- get unique elements of a list
unique :: (Eq a) => [a] -> [a]
unique xs = [x | (x, y) <- zip xs [0 ..], x `notElem` take y xs]

-- λ> canonize ['d', 'b', 'd', 'd', 'a']
--     [0, 1, 0, 0, 2]
-- λ> canonize ["zebra", "mouse", "zebra", "zebra", "owl"]
--     [0, 1, 0, 0, 2]


-- from the stdlib, putChar :: Char -> IO ()
-- We want to return (c1,c2, c3) inside of an IO monad.
-- Haskell provides "return" function for this purpose:
-- return :: a -> IO a
getTriple :: IO (Char, Char, Char)
getTriple =
    getChar >>= \x -> getChar >>= \y -> getChar >>= \z -> return (x, y, z)

putLine :: String -> IO ()
putLine []     = putChar '\n'
putLine (x:xs) = putChar x >> putLine xs

echoLine :: IO ()
echoLine = getLine >>= \cs -> putLine (reverse cs)
