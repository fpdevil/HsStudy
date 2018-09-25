-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy17
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    : Working through State Monad
--
-----------------------------------------------------------------------------
module HsStudy17
  (
    inc
  , incBy
  , testInc
  , tstState
  , fib
  , fibo
  , fibs
  , sumList
  , gcdS
  , incLabel
  , labelPair
  , labels
  , runLabels
  , string
  , render
  , tag
  , attrTag
  , doc
  , docenv
  , getPage
  , push
  , pop
  , stackOps
  , multipop
  , rolldie
  , rollDice
  , rollNDiceIO
  , rollNDice
  , playGame
  , game
  ) where

import           Control.Monad.State
import qualified Data.List           as L
import           System.Posix.Env
import           System.Random

inc :: State Int Int
inc = do
  n <- get
  put (n + 1)
  return n

incBy :: Int -> State Int Int
incBy n = do
  x <- get
  modify (+ n)
  return x

testInc :: IO ()
testInc = do
  print $ evalState inc 1
  print $ execState inc 1
  print $ runState inc 1
  print $ runState (withState (+3) inc) 1
  print $ runState (mapState (\(a, s) -> (a + 3, s + 4)) inc) 1
  print $ runState (incBy 5) 10

-- a simpler state monad tester
tstState :: Int -> (String, Int)
tstState = runState f
  where
    f = get >>= \x -> put (x * 10) >> return ['a' .. 'z']

-- λ> tstState 7
-- ("abcdefghijklmnopqrstuvwxyz",70)

-- Fibonacci numbers
fib :: Int -> State Int Int
fib 0 = return 1
fib 1 = return 1
fib n = do
  x <- fib (n - 1)
  y <- fib (n - 2)
  modify (+ 1)
  return (x + y)

-- λ> runState (fib 10) 0
-- (89,88)

fibo :: Int -> State (Integer, Integer, Int) Integer
fibo n = get >>=
         (\(a, b, s) -> if s == n
                       then return a
                       else put (b, a + b, s + 1) >> fibo n)

-- fibo n = get >>= (\(a, b, s) -> case s == n of
--                                  True -> return a
--                                  _    -> put (b, a + b, s + 1) >> fibo n)

-- λ> evalState (fibo 10) (1, 2, 1)
-- 89

fibs :: State (Integer, Integer) ()
fibs = get >>= (\(a, b) -> put (b, a + b))

-- λ> runState (replicateM 10 fibs) (0, 1)
-- ([(),(),(),(),(),(),(),(),(),()],(55,89))

-- add the list of integers
sumList :: [Int] -> State Int Int
sumList []       = get >>= \s -> return s
sumList (x : xs) = get >>= (\s -> put (s + x) >> sumList xs)

-- λ> runState (sumList [1..10]) 0
-- (55,55)

gcdS :: State (Integer, Integer) Integer
gcdS = get >>=
       (\(a, b) -> if b == 0 then put (a, a) >> return a
                  else
                    put (b, a `mod` b) >> gcdS)

-- gcdS = get >>= (\(a, b) -> case b == 0 of
--                             True -> put (a, a) >> return a
--                             _    -> put (b, a `mod` b) >> gcdS)
-- λ> evalState gcdS (1221, 1441)
-- 11


{-
                 generate a stream of unique labels
Let us  say we have a  requirement of generating labels  in code while
performing operations on an Abstract  Syntax Tree; for this each label
must be  unique and we  need  labels  in various functions;  since  in
haskell we cannot mutate the variables directly we have to control the
state of the variables
-}
type Label = State Int

-- increment the internal state and return a label
incLabel :: Label String
incLabel = state $ \x -> let y = x + 1
                            in ("$" ++ show y, y)

-- λ> :t runState incLabel 0
-- runState incLabel 0 :: (String, Int)
-- λ> runState incLabel 0
-- ("$1",1)
-- λ> runState incLabel 1
-- ("$2",2)


-- lift the above function to generate a pair of labels
labelPair :: Label (String, String)
labelPair = (,) <$> incLabel <*> incLabel

-- λ> runState labelPair 0
-- (("$1","$2"),2)
-- λ> runState ((,) <$> labelPair <*> labelPair) 0
-- ((("$1","$2"),("$3","$4")),4)


-- function for generating the labels
labels :: Bool -> Label [(String, String)]
labels predicate = func <$> labelPair
                        <*> labelPair
                        <*> labelPair
                        where
                          func x y z = if predicate
                                          then [x, z]
                                       else [x, y, z]

runLabels :: IO ()
runLabels = do
  putStrLn "Enter a predicate value: True or False"
  predicate <- getLine
  print $ evalState (labels . read $ predicate) 0

-- λ> runLabels
-- Enter a predicate value: True or False
-- False
-- [("$1","$2"),("$3","$4"),("$5","$6")]
-- λ> runLabels
-- Enter a predicate value: True or False
-- True
-- [("$1","$2"),("$5","$6")]

-- Library for producing a HTML
type HTML = State String

string :: String -> HTML ()
string s = modify (++ s)

-- λ> runState (string "Hello" >> string " World!") ""
-- ((),"Hello World!")

-- a function for rendering or displaying
render :: HTML a -> String
render mHTML = execState mHTML ""

-- λ> render (string "Hello" >> string " World!")
-- "Hello World!"

tag :: String -> HTML a -> HTML ()
tag t mHTML = do
  string $ "<" ++ t ++ ">"
  _ <- mHTML
  string $ "</" ++ t ++ ">"

attrTag :: String -> [(String, String)] -> HTML a -> HTML ()
attrTag t attrs mHTML = do
  let format (k, v) = k ++ "=" ++ show v
  string $ "<" ++ L.unwords (t : map format attrs) ++ ">"
  _ <- mHTML
  string $ "</" ++ t ++ ">"

-- define some tagging functions for standard html usage
html,hhead,title,body,p,i,b,hl,h2,h3,h4,ol,li,ul,table,tr,th,td ::
  HTML a -> HTML ()
html  = tag "html"
hhead  = tag "head"
title = tag "title"
body  = tag "body"
p     = tag "p"
i     = tag "i"
b     = tag "b"
hl    = tag "hl"
h2    = tag "h2"
h3    = tag "h3"
h4    = tag "h4"
ol    = tag "ol"
li    = tag "li"
ul    = tag "ul"
table = tag "table"
tr    = tag "tr"
th    = tag "th"
td    = tag "td"

-- provide a structural definition of an html page in Haskell
doc :: HTML ()
doc =
  html $
  hhead $ do
    title $ string "Hello, World!"
    body $ do
      hl $ string "Greetings!"
      p $ string "Hello, World!"

-- λ> render doc
-- "<html><head><title>Hello, World!</title><body><hl>Greetings!</hl><p>Hello, World!</p></body></head></html>"

-- get the environment information in HTML
docenv :: [(String, String)] -> HTML ()
docenv env =
  html $
  hhead $ do
    title $ string "Environment Variables:"
    body $ do
      hl $ string "Environment Variables:"
      ul $ mapM_ (\(x, y) -> li $ string $ x ++ "=" ++ y) $ L.sort env

getPage :: IO ()
getPage = do
  env <- getEnvironment
  putStr "Content-Type: text/html\n\n"
  putStr . render . docenv $ env

-- repeat monadic action
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x : xs)

stackOps :: State Stack Int
stackOps = pop >>= \x -> push 3 >> push 5 >> return x

-- λ> runState stackOps [10, 9 .. 6]
-- (10,[5,3,9,8,7,6])

multipop :: Int -> State Stack [Int]
multipop n = replicateM n pop

-- λ> runState (multipop 3) [10, 9 .. 1]
-- ([10,9,8],[7,6,5,4,3,2,1])

-- Dice Rolling
rolldie :: State StdGen Int
rolldie = do
  generator <- get
  let (value, newGenerator) = randomR (1, 6) generator
  put newGenerator
  return value

-- λ> evalState rolldie (mkStdGen 10)
-- 6

rollDice :: State StdGen (Int, Int)
rollDice = (,) <$> rolldie <*> rolldie

-- λ> evalState rollDice (mkStdGen 999)
-- (6,5)

-- given an integer, return a list with that number of pseudo-random
-- integers between 1 and 6
rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = replicateM n (randomRIO (1, 6))

-- without IO
rollNDice :: Int -> State StdGen [Int]
rollNDice n
  | n <= 0 = return []
  | otherwise = do
      generator <- get
      let (val, newGen) = randomR (1, 6) generator
      put newGen
      vals <- rollNDice (n - 1)
      return (val : vals)

-- λ> evalState (rollNDice 10) (mkStdGen 999)
-- [6,5,5,4,1,5,3,3,6,1]

{-
  State Game
 Passes a string of dictionary {a,b,c}
 Game is to produce a number from the string.
 By default the game is off, a C toggles the
 game on and off. A 'a' gives +1 and a b gives -1.
 E.g
 'ab'    = 0
 'ca'    = 1
 'cabca' = 0
 State = game is on or off & current score
       = (Bool, Int)
-}
type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
    (_, score) <- get
    return score
playGame (x : xs) = do
    (on, score) <- get
    case x of
        'a' | on -> put (on, score + 1)
        'b' | on -> put (on, score - 1)
        'c' -> put (not on, score)
        _   -> put (on, score)
    playGame xs

startState = (False, 0)

game = print $ evalState (playGame "abcaaacbbcabbab") startState
