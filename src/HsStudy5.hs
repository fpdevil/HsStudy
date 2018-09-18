{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy5
-- Copyright      :  (c) Some description... 2018
-- License        : BSD
--
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
--
-- Description    :
--
-----------------------------------------------------------------------------
module HsStudy5
  ( isValid
  , apply
  , ops
  , values
  , eval
  , subs
  , repeatedCombination
  , interleave
  , perms
  , choices
  , solver
  , split
  , resplit
  , combine
  , exprs
  , solutions
  , combiner
  , results
  , sols
  , fizzbuzz
  , showFB
  , solveRPN
  , xseqA
  , knightMove
  , moveIn3
  , mseq
  , mseq1
  , seqIO
  , printTri
  , repM
  , xfilterM
  , powerset
  , maxmin
  ) where

import Control.Applicative ()
import Control.Monad
import qualified Data.List as L

---------------------------------------------------------------------
-- The Countdown Problem
---------------------------------------------------------------------
-- Given a sequence of numbers and  a target number, attempt to construct
-- An expression  whose value  is the  target, by  combining one  or more
-- Numbers from the sequence using addition, subtraction, multiplication,
-- Division and parentheses.
data Op
  = Add -- ^ Addition
  | Sub -- ^ Subtraction
  | Mul -- ^ Multiplication
  | Div -- ^ Division

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

-- a function to check if the application of an operator over 2 positive
-- natural numbers yield another positive natural number. In reaching to
-- this definition,  we consider the following pointers of commutativity
-- and identity properties
-- ① A + B = B + A
-- ② A * B = B * A
-- ③ A * 1 = A ; 1 * B = B
-- ④ A ÷ 1 = A
isValid :: Op -> Int -> Int -> Bool
isValid Add x y = x <= y
isValid Sub x y = x > y
isValid Mul x y = x /= 1 && y /= 1 && x <= y
isValid Div x y = y /= 1 && x `mod` y == 0

-- a function for applying an operator over two integers
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- function for holding the list of available operations
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- test result
-- λ> ops
-- [ + , - , * , / ]
--
-- Numerical Expressions declaration
-- Declare a type which can either be an integer value or the application
-- of an operator to two argument expressions, together with a simple pretty
-- printer for expression rendering
data Expr
  = Val Int -- ^ Integer expression
  | Apply Op
          Expr
          Expr -- ^ Apply operator to two expressions

-- make Expr an instance of Show
instance Show Expr where
  show (Val n) = show n
  show (Apply o l r) = showE l ++ show o ++ showE r
    where
      showE (Val n) = show n
      showE e = "{ " ++ show e ++ " }"

-- function to return a list of values in an expression
values :: Expr -> [Int]
values (Val x) = [x]
values (Apply _ l r) = values l ++ values r

-- function to return the overall value of an expression as List
eval :: Expr -> [Int]
eval (Val x) = [x | x > 0]
eval (Apply op l r) =
  [apply op lx ry | lx <- eval l, ry <- eval r, isValid op lx ry]

-- Test Run
-- λ> (Apply Add (Val 2) (Apply Mul (Val 3) (Val 4)))
-- 2 + { 3 * 4 }
-- λ> values (Apply Add (Val 2) (Apply Mul (Val 3) (Val 4)))
-- [2,3,4]
-- λ> eval (Apply Add (Val 2) (Apply Mul (Val 3) (Val 4)))
-- [14]
---------------------------------------------------------------------
-- auxilliary helper combinatorial functions
---------------------------------------------------------------------
-- a function which returns all the subsequences of a list, which are given
-- by all possible  combinations of excluding  or including each element of
-- the list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
  where
    yss = subs xs

repeatedCombination :: [a] -> Int -> [[a]]
repeatedCombination _ 0 = [[]]
repeatedCombination xs n =
  [x : y | x <- xs, y <- repeatedCombination xs (n - 1)]

-- λ> repeatedCombination "abc" 2
-- ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]
-- function to return all possible values of inserting an element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

-- λ> interleave '#' "abc"
-- ["#abc","a#bc","ab#c","abc#"]
-- permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (interleave x) (perms xs)

-- function for getting all possible ways of selecting zero or
-- more elements from a list
choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- a predicate to solve an instance of the Countdown
solver :: Expr -> [Int] -> Int -> Bool
solver ex src target = (values ex) `elem` (choices src) && eval ex == [target]

-- function to return all possible values from a list after splitting into
-- two non-empty lists
split :: [a] -> [([a], [a])]
split [] = [([], [])]
split (x:xs) = ([], x : xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- remove either of the empty lists from the split
resplit :: [a] -> [([a], [a])]
resplit = filter (\(x, y) -> not (null x || null y)) . split

-- combine expressions
combine :: Expr -> Expr -> [Expr]
combine l r = [Apply op l r | op <- ops]

-- return list of all expressoins whose values are precisely a given
-- list of integers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [x] = [Val x]
exprs xs =
  [ex | (ls, rs) <- resplit xs, l <- exprs ls, r <- exprs rs, ex <- combine l r]

-- return all possible solutions for an instance of the Problem
solutions :: [Int] -> Int -> [Expr]
solutions xs x = [ex | cs <- choices xs, ex <- exprs cs, eval ex == [x]]

-- a more efficient approoch
-- listing together generation and evaluation at the same time, which would
-- allow for the rejection of invalid expressions early
type Result = (Expr, Int)

-- combine expressions for Result
combiner :: Result -> Result -> [Result]
combiner (l, x) (r, y) =
  [(Apply op l r, apply op x y) | op <- ops, isValid op x y]

-- get an evaluated pair list of expression and value from the input list
results :: [Int] -> [Result]
results xs = [(ex, x) | ex <- exprs xs, x <- eval ex]

-- test run
-- λ> results [2,1]
-- [(2 + 1,3),(2 - 1,1),(2 * 1,2),(2 / 1,2)]
sols :: [Int] -> Int -> [Expr]
sols src target = [ex | cs <- choices src, (ex, i) <- results cs, i == target]

-- Fizzbuzz problem
fb :: Int -> String
fb x
  | isDivisibleBy x 15 = "FizzBuzz"
  | isDivisibleBy x 5 = "Fizz"
  | isDivisibleBy x 3 = "Buzz"
  | otherwise = show x
  where
    isDivisibleBy a b = a `mod` b == 0

fizzbuzz :: IO ()
fizzbuzz = mapM_ (putStrLn . fb) [1 .. 100]

-- an alternate implementation of fizzbuzz with fibonacci
fizzBuzz :: Int -> String
fizzBuzz n =
  case (n `mod` 3 == 0, n `mod` 5 == 0) of
    (True, True) -> "FizzBuzz"
    (True, False) -> "Fizz"
    (False, True) -> "Buzz"
    (_, _) -> show n

fibonacciSeries :: Int -> Int -> Int -> Int
fibonacciSeries n x y
  | n < 0 = x
  | otherwise = fibonacciSeries (n - 1) y (x + y)

fibs :: Int -> Int
fibs n = fibonacciSeries n 0 1

showFB :: Int -> [String]
showFB n = map (fizzBuzz . fibs) [1 .. n]

-- test run
-- λ> mapM_ print $ showFB 10
-- "1"
-- "2"
-- "Fizz"
-- "Buzz"
-- "8"
-- "13"
-- "Fizz"
-- "34"
-- "Buzz"
-- "89"
-------------------------------------------------------------------------------
-- a simple RPN calculator implementation
-------------------------------------------------------------------------------
foldingFun :: (Num a, Read a) => [a] -> String -> [a]
foldingFun (x:y:ys) "*" = (y * x) : ys
foldingFun (x:y:ys) "+" = (y + x) : ys
foldingFun (x:y:ys) "-" = (y - x) : ys
foldingFun xs numStr = read numStr : xs

solveRPN :: (Num a, Read a) => String -> a
solveRPN expr = head $ foldl foldingFun [] (L.words expr)

xseqA :: (Applicative f) => [f a] -> f [a]
xseqA [] = pure []
xseqA (x:xs) = pure (:) <*> x <*> xseqA xs

-------------------------------------------------------------------------------
-- Knight's quest
-------------------------------------------------------------------------------
type KnightPos = (Int, Int)

-- take a knight's position and return all it's possible moves
knightMove :: KnightPos -> [KnightPos]
knightMove (x, y) = do
  (c, r) <-
    [ (x + 1, y + 2)
    , (x + 1, y - 2)
    , (x - 1, y + 2)
    , (x - 1, y - 2)
    , (x + 2, y + 1)
    , (x + 2, y - 1)
    , (x - 2, y - 1)
    , (x - 2, y + 1)
    ]
  guard (c `elem` [1 .. 8] && r `elem` [1 .. 8])
  return (c, r)

-- positiona which may be reachinghed in 3 steps
moveIn3 :: KnightPos -> [KnightPos]
moveIn3 start = do
  first <- knightMove start
  second <- knightMove first
  knightMove second

-------------------------------------------------------------------------------
-- the function takes a list of monadic computations, executes one by one and returns
-- a list of the results; if any computation fails, the whole function fails
mseq :: (Monad m) => [m a] -> m [a]
mseq = foldr mcons (return [])
  where
    mcons x y = x >>= \l -> y >>= \m -> return (l : m)

mseq1 :: (Monad m) => [m a] -> m [a]
mseq1 [] = return []
mseq1 (x:xs) = x >>= (\y -> mseq1 xs >>= \ys -> return (y : ys))

-- sequence over IO
seqIO :: [IO a] -> IO [a]
seqIO [] = return []
seqIO (x:xs) = do
  y <- x
  ys <- seqIO xs
  return (y : ys)

printTri :: Int -> IO ()
printTri n = mapM_ (print . concat . flip replicate "x") [1 .. n]

{- λ> printTri 7
 - "x"
 - "xx"
 - "xxx"
 - "xxxx"
 - "xxxxx"
 - "xxxxxx"
 - "xxxxxxx" -}
-- Monadic replicate
repM :: (Monad m) => Int -> m a -> m [a]
repM n x = sequence (replicate n x)

-- filterM re-implementation
xfilterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
xfilterM _ [] = return []
xfilterM f (x:xs) = do
  y <- f x
  ys <- xfilterM f xs
  return
    (if y
       then x : ys
       else ys)

-- powerset definition
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- λ> powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
-- get a range of max,min from a List
maxmin :: (Ord a) => [a] -> (a, a)
maxmin [x] = (x, x)
maxmin (x:xs) =
  ( if x > max_xs
      then x
      else max_xs
  , if x < min_xs
      then x
      else min_xs)
  where
    (max_xs, min_xs) = maxmin xs
-- λ> maxmin [3,2,5,9,12,4]
-- (12,2)
