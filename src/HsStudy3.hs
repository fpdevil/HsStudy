{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy3
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    : Some Monadic examples
--
-----------------------------------------------------------------------------
module HsStudy3
(
    moves
  , getMoves
  , hanoi
  , powerOf
  , fibw
  , seriesUp
) where

import           Control.Monad.Writer

-- Towers of Hanoi
-- define data types for the 3 Pegs
data Peg = A | B | C

instance Show Peg where
  show A = "A"
  show B = "B"
  show C = "C"

-- for a 3 pegged Towers Of Hanoi, we can categorize the pegs as follows
-- Source or Starting Peg         : src
-- Target or Destination Peg      : target
-- Auxilliary or Intermediate Peg : aux

-- define a function move for handling the movement of pegs
moves :: Int -> Peg -> Peg -> Peg -> Writer [String] Int
moves pegs src aux target
  | pegs == 1 = writer (1, [show src ++ " ==> " ++ show target])
  | otherwise = do
    x <- moves (pegs - 1) src target aux
    _ <- moves 1 src aux target
    y <- moves (pegs - 1) aux src target
    return (x + y + 1)

-- define a rendering function for getting and displaying the results
getMoves :: (Int, [String]) -> IO ()
getMoves (n, steps) = do
  putStrLn "------------"
  putStrLn $ "Solved Towers Of HANOI with " ++ show n ++ " moves!!!"
  mapM_ print steps
  putStrLn "------------"

hanoi :: Int -> IO ()
hanoi n = getMoves $ runWriter (moves n A B C)

-- λ> hanoi 3
-- ------------
-- Solved Towers Of HANOI with 7 moves!!!
-- "A ==> C"
-- "A ==> B"
-- "C ==> B"
-- "A ==> C"
-- "B ==> A"
-- "B ==> C"
-- "A ==> C"
-- ------------

-- calculation of power of one number over other
-- x to the power of n = xⁿ
powerOf :: Int -> Int -> Writer [String] Int
powerOf _ 0 = return 1
powerOf x n
  | even n = powerOf x (n `div` 2) >>=
             \y -> writer (y * y, ["squaring the even power " ++ show y ++ " * " ++ show y])
  | otherwise = powerOf x (n - 1) >>=
                \y -> writer (x * y, ["multiplying for odd power " ++ show x ++ " * " ++ show y])

-- λ> fst . runWriter $ 4 `powerOf` 5
-- 1024
-- λ> mapM_ print $ execWriter $ 4 `powerOf` 5
-- "multiplying for odd power 4 * 1"
-- "squaring the even power 4 * 4"
-- "squaring the even power 16 * 16"
-- "multiplying for odd power 4 * 256"

-- fibonacci numbers with logging
--
fibw :: Int -> Writer [String] Int
fibw n
  | n < 2 =
    do tell ["fibw " ++ show n ++ " = 1"]
       return 1
  | otherwise =
    do a <- fibw (n - 1)
       b <- fibw (n - 2)
       tell ["fibw " ++ show n ++ "[" ++ show a ++ " + " ++ show b ++ "]" ++ " = " ++ show (a + b)]
       return (a + b)

{- λ> mapM_ print . execWriter $ fibw 5
 - "fibw 1 = 1"
 - "fibw 0 = 1"
 - "fibw 2[1 + 1] = 2"
 - "fibw 1 = 1"
 - "fibw 3[2 + 1] = 3"
 - "fibw 1 = 1"
 - "fibw 0 = 1"
 - "fibw 2[1 + 1] = 2"
 - "fibw 4[3 + 2] = 5"
 - "fibw 1 = 1"
 - "fibw 0 = 1"
 - "fibw 2[1 + 1] = 2"
 - "fibw 1 = 1"
 - "fibw 3[2 + 1] = 3"
 - "fibw 5[5 + 3] = 8" -}


-- seriesUp
-- Given n>=0, create an array with the pattern {1, 1, 2, 1, 2, 3, ... 1,
-- 2, 3 .. n}  (spaces added to show the grouping).  Note that the length
-- of the  array will be  1 + 2  + 3 ...  + n, which  is known to  sum to
-- exactly n*(n + 1)/2.
seriesUp :: Int -> [[Int]]
seriesUp n = map (\x -> [1 .. x]) [1 .. n]

-- sample run
{- λ> mapM_ print $ seriesUp 5
 - [1]
 - [1,2]
 - [1,2,3]
 - [1,2,3,4]
 - [1,2,3,4,5] -}
