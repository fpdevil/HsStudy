-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy15
-- Copyright   :
-- License     :
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 15
-- ghc --make -Wall HsStudy15.hs
-- Tackling with Writer Monad(s)
-----------------------------------------------------------------------------
module HsStudy15
  (
    applyLog
  , addDrink
  , logNumber
  , multWithLog
  , binPower
  , gcdx
  , fibNextW
  , fastFibW
  , moveDiscs
  , showMoves
  , hanoi
  , deleteOn
  , logMore
  ) where

import           Control.Monad.Writer
import           Data.Monoid

{-
repl examples

λ> return 5 :: Writer String Int
WriterT (Identity (5,""))
λ> return 5 :: Writer [String] Int
WriterT (Identity (5,[]))
λ> return 5 :: Writer (Sum Int) Int
WriterT (Identity (5,Sum {getSum = 0}))
λ> return 5 :: Writer (Product Int) Int
WriterT (Identity (5,Product {getProduct = 1}))

λ> tell [1] >> tell [2] >> tell [3] :: Writer [Int] ()
WriterT (Identity ((),[1,2,3]))
λ> tell [1] >> tell [2] >> tell [3] >> return "Done" :: Writer [Int] String
WriterT (Identity ("Done",[1,2,3]))

λ> tell "abc" >> tell "def" >> tell "ghi" :: Writer String ()
WriterT (Identity ((),"abcdefghi"))
λ> tell "abc" >> tell "def" >> tell "ghi" >> return [1..3] :: Writer String [Int]
WriterT (Identity ([1,2,3],"abcdefghi"))

λ> tell [1..5] >> return "Done" :: Writer [Int] String
WriterT (Identity ("Done",[1,2,3,4,5]))
λ> runWriter $ tell [1..5] >> return "Done"
("Done",[1,2,3,4,5])
λ> execWriter $ tell [1..5] >> return "Done"
[1,2,3,4,5]

-}

-- starting with LYAH Writer Monad
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = (y, log `mappend` ylog)
  where
    (y, ylog) = f x

-- a small example
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "chocolates" = ("Milk Shake", Sum 12)
addDrink "bread"      = ("Beer", Sum 9)
addDrink _            = ("Water", Sum 21)

-- logging of number multiplication
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  x <- logNumber 5
  y <- logNumber 6
  tell ["multiplying two numbers"]
  return (x * y)

-- λ> runWriter multWithLog
-- (30,["Got number: 5","Got number: 6"])

{-
using tell
λ> :t tell [1] >> tell [2] >> tell [3]
   tell [1] >> tell [2] >> tell [3] :: (Num t, MonadWriter [t] m) => m ()
λ> :t tell [1] >> tell [2] >> tell [3] :: Writer [Int] ()
   tell [1] >> tell [2] >> tell [3] :: Writer [Int] () :: Writer [Int] ()
λ> tell [1] >> tell [2] >> tell [3] :: Writer [Int] ()
   WriterT (Identity ((),[1,2,3]))
λ> runWriter $ tell [1] >> tell [2] >> tell [3]
   ((),[1,2,3])
-}

-- Binary Powers
binPower :: Int -> Int -> Writer [String] Int
binPower 0 _ = return 1
binPower n x
  | even x = binPower (n `div` 2) x >>= \y ->
             writer (x * x, ["Square " ++ show y])
  | otherwise = binPower (n - 1) x >>= \y ->
                writer (x * y, ["Multiplying " ++ show x ++ " & " ++ show y])

{-
λ> mapM_ print $ execWriter $ binPower 5 6 >> binPower 4 3
"Square 1"
"Square 36"
"Square 36"
"Multiplying 3 & 1"
"Multiplying 3 & 3"
"Multiplying 3 & 9"
"Multiplying 3 & 27"
-}

-- GCD of 2 numbers
gcdx :: Int -> Int -> Writer [String] Int
gcdx x y
  | y == 0 = do
      tell ["finished with " ++ show x]
      return x
  | otherwise = do
      tell [show x ++ " mod " ++ show y ++ " = " ++ show (x `mod` y)]
      gcdx y (x `mod` y)

{-
λ> mapM_ print $ execWriter $ gcdx 1221 2044
"1221 mod 2044 = 1221"
"2044 mod 1221 = 823"
"1221 mod 823 = 398"
"823 mod 398 = 27"
"398 mod 27 = 20"
"27 mod 20 = 7"
"20 mod 7 = 6"
"7 mod 6 = 1"
"6 mod 1 = 0"
"finished with 1"
-}

-- fibonacci numbers with Writer Monad
fibNextW :: (Num a, Show a) => (a, a) -> Writer [String] (a, a)
fibNextW (x, y) = do
  tell ["Using x = " ++ show x ++ " & y = " ++ show y]
  return (y, x + y)

fastFibW :: (Num a, Ord a, Show a) => a -> Writer [String] (a, a)
fastFibW n
  | n < 0 = do
      tell ["Negative number is not allowed"]
      return (0, 0)
  | n == 0 = do
      tell ["returning default pair of (1, 1)"]
      return (1, 1)
  | otherwise = do
      l <- fastFibW (n - 1)
      m <- fibNextW l
      tell ["returning the value " ++ show m]
      return m

{-
λ> fst . fst . runWriter . fastFibW $ 10
89

λ> mapM_ print $ (execWriter . fastFibW $ 10)
"returning default pair of (1, 1)"
"Using x = 1 & y = 1"
"returning the value (1,2)"
"Using x = 1 & y = 2"
"returning the value (2,3)"
"Using x = 2 & y = 3"
"returning the value (3,5)"
"Using x = 3 & y = 5"
"returning the value (5,8)"
"Using x = 5 & y = 8"
"returning the value (8,13)"
"Using x = 8 & y = 13"
"returning the value (13,21)"
"Using x = 13 & y = 21"
"returning the value (21,34)"
"Using x = 21 & y = 34"
"returning the value (34,55)"
"Using x = 34 & y = 55"
"returning the value (55,89)"
"Using x = 55 & y = 89"
"returning the value (89,144)"
-}

-- Towers Of Hanoi using Writer Monad
-- using 3 Pegs A, B, C
-- To move n discs from peg A to peg B:
--   1. move top n−1 discs from A to C via B. This leaves disc #n alone on peg A
--   2. move the remaining disc #n from A to B
--   3. move n−1 discs from C to B via A
data Peg = A | B | C

instance Show Peg where
  show A = "Peg A"
  show B = "Peg B"
  show C = "Peg C"

instance Eq Peg where
  A == A = True
  B == B = True
  C == C = True
  _ == _ = False

-- function for handling the disk movement across the pegs
-- for a 3 pegged Towers Of Hanoi, we can categorize the pegs as follows
-- Source or Starting Peg         : src
-- Target or Destination Peg      : target
-- Auxilliary or Intermediate Peg : aux
moveDiscs :: Int -> Peg -> Peg -> Peg -> Writer [String] Int
moveDiscs n src aux target
  | n == 0 = writer (0, ["no possible moves"])
  | n == 1 = writer (1, [show src ++ " ==> " ++ show target])
  | otherwise = do
      x <- moveDiscs (n - 1) src target aux
      _ <- moveDiscs 1 src aux target
      y <- moveDiscs (n - 1) aux src target
      return (1 + x + y)

-- function for rendering the results
showMoves :: (Int, [String]) -> IO ()
showMoves (n, steps) =
  do putStrLn "-------------------------------------------"
     putStrLn $ "Towers Of Hanoi solved with " ++ show n ++ " moves!!!"
     mapM_ print steps
     putStrLn "-------------------------------------------"

hanoi :: Int -> IO ()
hanoi n = showMoves . runWriter $ moveDiscs n A B C

{-
λ> hanoi 3
-------------------------------------------
Towers Of Hanoi solved with 7 moves!!!
"Peg A ==> Peg C"
"Peg A ==> Peg B"
"Peg C ==> Peg B"
"Peg A ==> Peg C"
"Peg B ==> Peg A"
"Peg B ==> Peg C"
"Peg A ==> Peg C"
-------------------------------------------
-}

{-
Inside a Writer you can't inspect what has been written, until you run
(or "unwrap") the  monad, using execWriter or  runWriter. However, you
can use  listen to inspect what  some sub-action wrote to  the writer,
before the value is appended to the writer state, and you can use pass
to modify what is written.

Ex: Log an action only if the log produced by the Writer satisifies a
predicate condition.
-}
deleteOn :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn predicate mw = pass $ do
  (a, w) <- listen mw
  if predicate w
      then return (a, id)
      else return (a, const mempty)

logMore :: Writer [String] ()
logMore = do
  deleteOn ((> 5) . length . head) $ tell ["basket"]
  deleteOn ((> 5) . length . head) $ tell ["ball"]
  deleteOn ((== 3) . length . head) $ tell ["pie"]
  deleteOn (\x -> x == reverse x) $ tell ["abracadabra"]

-- test run
-- λ> logTwice
-- WriterT (Identity ((),["basket","pie","abracadabra"]))
-- λ> runWriter logTwice
-- ((),["basket","pie","abracadabra"])
