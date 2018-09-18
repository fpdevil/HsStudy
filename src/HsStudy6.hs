{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module         : HsStudy6
-- Copyright      :  (c) Some description... 2018
--
-- License        : BSD
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    :
--
-----------------------------------------------------------------------------

module HsStudy6
       (
         positions
       , apply
       , ops
       , choices
       , combine
       , combiner
       , exprs
       , getResult
       , getVal
       , getVals
       , isValid
       , perms
       , results
       , subseqs
       , solver
       , selections
       , split
       , main
       )
       where

import System.Environment (getArgs)


-- function to return all positions of a value in a list
positions :: (Eq a) => a -> [a] -> [Int]
positions n xs = [i | (i, x) <- zip [0 .. l] xs, n == x]
          where
            l = length xs - 1

-- λ> positions 'a' "abracadabra"
-- [0,3,5,7,10]


-- Another approach to solving the CountDown problem
-- To find all the ways of combining numbers to yield a certain result we
-- have to generate all possible ways of combining numbers (subproblem 1)
-- and then filter  out the ones with the desired  output (subproblem 2).
-- The  pattern of  all possible  ways of  generating valid  mathematical
-- expressions with the numbers can be expressed as a Binary Tree.


-- define a data type for holding the template for mathematical experssions
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

-- apply a binary operator over two Numbers
apply :: Op -> Int -> Int -> Int
apply Add a b = a + b
apply Sub a b = a - b
apply Mul a b = a * b
apply Div a b = a `div` b

-- list of all available operators
ops :: [Op]
ops = [Add, Sub, Mul, Div]

data Exp = Val Int           -- ^ An integer expression
         | Do Op Exp Exp     -- ^ Do apply operator over 2 expressions

instance Show Exp where
  show (Val x) = show x
  show (Do op l r) = showExp l ++ show op ++ showExp r
    where
      showExp (Val v) = show v
      showExp e       = "{ " ++ show e ++ " }"

-- λ> show (Do Add (Val 3) (Do Mul (Val 4) (Val 5)))
-- "3 + { 4 * 5 }"

-- check for the operator validity in an expession
isValid :: Op -> Int -> Int -> Bool
isValid Add a b = a <= b
isValid Sub a b = a > b
isValid Mul a b = a /= 1 && b /= 1 && a <= b
isValid Div a b = b /= 1 && a `mod` b == 0

-- return value from a single expression
getVal :: Exp -> Int
getVal (Val x)       = x
getVal (Do op e1 e2) = apply op (getVal e1) (getVal e2)

-- return a list of integer values from an Abstract expression
getVals :: Exp -> [Int]
getVals (Val x)    = [x]
getVals (Do _ x y) = getVals x ++ getVals y

-- get the result of computation from an abstract expression
-- this either succeeds and returns a singleton list, or fails
-- and returns an empty list
getResult :: Exp -> [Int]
getResult (Val x)     = [x | x > 0]
getResult (Do op l r) = [apply op lx ry | lx <- getResult l,
                                          ry <- getResult r,
                                          isValid op lx ry]

-- generate all possible ways to select an element from a list by keeping
-- track of which items have not yet been used
selections :: [a] -> [(a, [a])]
selections []       = []
selections (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- selections xs]

-- λ> selections "abc"
-- [('a',"bc"),('b',"ac"),('c',"ab")]

-- return nonempty subsequences of a nonempty list
subseqs :: [x] -> [[x]]
subseqs [] = [[]]
subseqs (x : xs) = yss ++ map (x :) yss
        where
          yss = subseqs xs

-- λ> subseqs "abc"
-- ["","c","b","bc","a","ac","ab","abc"]

-- permutations of a list of elements
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (ndinsert x) (perms xs)
  where
    -- non-deterministic insertion in list
    ndinsert :: a -> [a] -> [[a]]
    ndinsert z []         = [[z]]
    ndinsert z q@(y : ys) = (z : q) : map (y :) (ndinsert z ys)

-- return a list of all possible ways of splitting a non empty list
split :: [a] -> [([a], [a])]
split zs = filter (\(x, y) -> not (null x || null y)) (xsplit zs)
      where
        xsplit :: [a] -> [([a], [a])]
        xsplit []       = [([], [])]
        xsplit (x : xs) = ([], x : xs) : [(x : ls, rs) | (ls, rs) <- xsplit xs]

-- λ> split "abcd"
-- [("a","bcd"),("ab","cd"),("abc","d")]

-- define a combinatorial function to return all choices from a list
-- given by all the possible ways of selecting zero or more elements
-- in any order
choices :: [a] -> [[a]]
choices xs = [ys | zs <- subseqs xs
                 , ys <- perms zs]


-- combine two expressions
combine :: Exp -> Exp -> [Exp]
combine e1 e2 = [Do op e1 e2 | op <- ops]

-- return a list of all possible expressions whose values are precisely
-- a given list of Numbers
exprs :: [Int] -> [Exp]
exprs [] = []
exprs [x] = [Val x]
exprs xs = [e | (ls, rs) <- split xs
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

-- define a type for holding the valid expressions and their values
type Result = (Exp, Int)

-- with the above definition of Result, we can define another function
-- for combining the results as below
combiner :: Result -> Result -> [Result]
combiner (e1, x) (e2, y) = [(Do op e1 e2, apply op x y) | op <- ops
                                                        , isValid op x y]

-- define a function that fuses together the generation and evaluation
-- of expression
results :: [Int] -> [Result]
results [] = []
results [x] = [(Val x, x) | x > 0]
results xs = [res | (ls, rs) <- split xs
                  , lx <- results ls
                  , ry <- results rs
                  , res <- combiner lx ry]

solver :: [Int] -> Int -> [Exp]
solver xs n = [ex | ys <- choices xs
                  , (ex, m) <- results ys
                  , m == n]

main :: IO ()
main = do
  (target : source) <- getArgs
  let a = read target :: Int
      b = read (head source) :: [Int]
  mapM_ print $ solver b a
