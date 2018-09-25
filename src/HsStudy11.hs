-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy11
-- Copyright   :
-- License     :  MIT
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 10
-- ghc --make -Wall HsStudy10.hs
--
-----------------------------------------------------------------------------
module HsStudy11
  (
    powerset
  , toss
  , pick
  , experiment
  , experimentM
  , fixedPoint
  , fixedPointVerbose
  , goldenRatio
  , isAdjacent
  , isUnique
  , persons
  , lives
  , whoLivesWhere
  , solution
  ) where

import           Control.Monad

-- powerset
powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs >>= \set -> [set, x : set]

-- λ> powerset [1,2,3]
-- [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

{-
You have two  coins, labeled Biased and Fair. The  Biased coin has two
heads, and the Fair coin has one  head and one tail. Pick one of these
coins at random,  toss it and observe  the result. If the  result is a
head, what is the probability that you picked the Biased coin?
-}
data Coin = Biased | Fair

instance Show Coin where
  show Biased = "Biased Coin"
  show Fair   = "Fair Coin"

instance Eq Coin where
  Biased == Biased = True
  Fair == Fair = True
  _ == _ = False

data CoinFace = Tail | Head deriving (Show)

instance Eq CoinFace where
  Tail == Tail = True
  Head == Head = True
  _ == _ = False

-- Toss a Coin
toss :: Coin -> [CoinFace]
toss Biased = [Head, Head]
toss Fair   = [Tail, Head]

-- Pick a coin randomly
pick :: [Coin]
pick = [Fair, Biased]

-- get the probability of picking a Biased Coin
experiment :: [Coin]
experiment = do
  p <- pick
  t <- toss p
  guard $ t == Head
  return p

-- OR in a more monadic way
experimentM :: [Coin]
experimentM =
  pick >>= \coin ->
  toss coin >>= \result ->
  guard (result == Head) >>
  return coin


-- λ> experiment
-- [Fair Coin,Biased Coin,Biased Coin]
-- So, there is a 2/3 chance for Biased and 1/3 chance for Fair coins



{-
  From SICP (1.3.3)
  Finding fixed points of functions

A number x is called a fixed point  of a function f if x satisfies the
equation f (x) =  x. For some functions f we can  locate a fixed point
by beginning with  an initial guess and applying f  repeatedly, like f
(x), f (f (x)), f (f (f (x))), . . . , until the value does not change
very much.

Using this idea,  we can devise a procedure fixed-point  that takes as
inputs a function  and an initial guess and  produces an approximation
to a  fixed point of  the function.  We apply the  function repeatedly
until we find two successive values whose difference is less than some
prescribed tolerance:
-}
fixedPoint :: (Ord a, Fractional a) => (a -> a) -> a -> a
fixedPoint f firstGuess = try firstGuess
  where
    try guess = let next = f guess in
                    if closeEnough guess next
                        then next
                        else try next

closeEnough :: (Num a, Ord a, Fractional a) => a -> a -> Bool
closeEnough v1 v2 = abs (v1 - v2) < tolerance
  where
    tolerance = 0.00001

fixedPointVerbose :: (Ord a, Fractional a, Show a) => (a -> a) -> a -> IO a
fixedPointVerbose f firstGuess = try firstGuess 1
  where
    try guess n = do
      putStrLn $ "guess " ++ show n ++ ": " ++ show guess
      let next = f guess
      if closeEnough guess next
          then return next
          else try next (n + 1)

goldenRatio :: (Ord a, Fractional a) => a
goldenRatio = fixedPoint (\x -> 1 + 1/x) 1.0

{-
        SICP #4.3.2 Examples of Nondeterministic Programs

Baker, Cooper, Fletcher, Miller, and Smith live on different floors of
an apartment house that contains only five floors. Baker does not live
on the top  floor. Cooper does not live on  the bottom floor. Fletcher
does not live on either the top or the bottom floor. Miller lives on a
higher floor than does Cooper. Smith does not live on a floor adjacent
to Fletcher’s. Fletcher does not live on a floor adjacent to Cooper’s.
Where does everyone live?
-}

-- create types for keeping the Name and Floor
type Name = String
type Floor = Int

-- create a data type for keeping the name of the person and
-- the position or the floor on which they are
data Position = Position { person :: Name
                         , floorN :: Floor
                         }

instance Show Position where
  show p = "Mr." ++ show (person p) ++ " lives on the floor no." ++ show (floorN p)

-- define a function to represent if 2 persons are adjacent to each other
isAdjacent :: Floor -> Floor -> Bool
isAdjacent floorA floorB = abs (floorA - floorB) == 1

-- check if all the elements of a list are distinct or unique
isUnique :: (Ord a) => [a] -> Bool
isUnique []       = True
isUnique [_]      = True
isUnique (x : xs) = (x `notElem` xs) && isUnique xs

persons :: [Name]
persons = ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]

lives :: [[Int]]
lives = do
  baker    <- [1 .. 5]
  cooper   <- [1 .. 5]
  fletcher <- [1 .. 5]
  miller   <- [1 .. 5]
  smith    <- [1 .. 5]
  guard $ isUnique [baker, cooper, fletcher, miller, smith]
  guard (baker /= 5)
  guard (cooper /= 1)
  guard (fletcher /= 5 && fletcher /= 1)
  guard (miller > cooper)
  guard $ not (smith `isAdjacent` fletcher)
  guard $ not (fletcher `isAdjacent` cooper)
  return [baker, cooper, fletcher, miller, smith]

whoLivesWhere :: [Position]
whoLivesWhere = do
  (x, y) <- persons `zip` concat lives
  return Position { person = x, floorN = y }

solution :: IO ()
solution = mapM_ print whoLivesWhere

{-
          Test Run

λ> solution
Mr."Baker" lives on the floor no.3
Mr."Cooper" lives on the floor no.2
Mr."Fletcher" lives on the floor no.4
Mr."Miller" lives on the floor no.5
Mr."Smith" lives on the floor no.1
-}
