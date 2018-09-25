-- |
-- Module         : HsStudy7
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    :
--
-----------------------------------------------------------------------------
module HsStudy7
    ( color
    , getRank
    , getSuit
    , isHigherRank
    , listRanks
    , sameSuit
    , table
    , swap
    , canCardBeat
    , canHandBeat
    , getLowestCard
    , betterCards
    , remove
    , ndinsert
    , divSub
    , concatNums
    , foldr
    , foldl
    , showFoldr
    , showFoldl
    , move
    , hanoi
    , printMoves
    , getRecursiveContents
    , add
    , view
    , purge
    , guess
    ) where

import           Control.Monad
import qualified Data.List          as L
import           Data.Maybe
import           Prelude            hiding (foldl, foldr)
import           System.Directory
import           System.Environment
import           System.FilePath    ((</>))
import           System.IO
import           System.Random

-- modelling of playing cards
data Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs
    deriving (Ord, Bounded)

-- Cards are enumerated with all clubs first, then diamonds, hearts, and spades
instance Show Suit where
    show Spades   = "♠"
    show Hearts   = "♥"
    show Diamonds = "♦"
    show Clubs    = "♣"

instance Eq Suit where
    Spades == Spades = True
    Hearts == Hearts = True
    Diamonds == Diamonds = True
    Clubs == Clubs = True
    _ == _ = False

table :: [(Suit, Int)]
table = [(Spades, 0), (Hearts, 1), (Diamonds, 2), (Clubs, 3)]

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

instance Enum Suit where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)

-- each suit has a color of either red or black
data Color
    = Black
    | Red
    deriving (Ord, Bounded)

instance Show Color where
    show Black = "Black"
    show Red   = "Red"

instance Eq Color where
    Black == Black = True
    Red == Red = True
    _ == _ = False

-- get colors of suit
color :: Suit -> Color
color Spades   = Black
color Clubs    = Black
color Hearts   = Red
color Diamonds = Red

-- Rank of Cards
-- Cards have ranks from 2,3 ... 10,J,Q,K,A
data Rank
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "10"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

-- list all the card ranks
listRanks :: [Rank]
listRanks = [minBound .. maxBound]

-- check if one rank is higher than the other rank
isHigherRank :: Rank -> Rank -> Bool
isHigherRank Ace _   = True
isHigherRank _ Ace   = False
isHigherRank King _  = True
isHigherRank _ King  = False
isHigherRank Queen _ = True
isHigherRank _ Queen = False
isHigherRank Jack _  = True
isHigherRank _ Jack  = False
isHigherRank r1 r2   = r1 > r2

-- A Card can have both a Rank as well as a Suit
data Card = Card
    { rank :: Rank
    , suit :: Suit
    } deriving (Eq, Bounded)

-- get rank and suit out of Card
getRank :: Card -> Rank
getRank (Card r _) = r

getSuit :: Card -> Suit
getSuit (Card _ s) = s

instance Show Card where
    show (Card r s) = show r ++ show s

instance Ord Card where
    compare (Card r1 s1) (Card r2 s2) =
        let suitOrder = compare s1 s2
        in if suitOrder == EQ                   -- check if they are from same suit
               then compare r1 r2               -- and then compare their ranks
               else suitOrder                   -- else return the comparision result of suits

instance Enum Card where
    fromEnum (Card r s) = fromEnum s * 13 + fromEnum r
    toEnum n =
        let r = toEnum (n `div` 13)
            s = toEnum (n `div` 13)
        in Card r s

-- λ> Card Ace Diamonds
-- A♦
-- λ> Card Queen Clubs
-- Q♣
-- When can one card beat another card?
-- If both the cards are from same suit and ranks are higher
canCardBeat :: Card -> Card -> Bool
canCardBeat (Card r1 s1) (Card r2 s2) = s1 == s2 && isHigherRank r1 r2

-- Modelling of Cards in Hand
-- A hand can be empty without any cards at all or it may hold one
-- more card into an existing card stack
data Hand
    = Empty
    | Add Card
          Hand

instance Show Hand where
    show Empty     = "Ø"
    show (Add c h) = show c ++ " on " ++ show h

-- get a hand with all cards from the same Suit
sameSuit :: Hand -> Suit -> Hand
sameSuit Empty _ = Empty
sameSuit (Add c h) s
    | getSuit c == s = Add c (sameSuit h s)
    | otherwise = sameSuit h s

-- check if a hand can beat Card
-- An empty hand can beat nothing, a non-empty hand can beat a card if
-- the first card can, or if the rest of the hand can
canHandBeat :: Hand -> Card -> Bool
canHandBeat Empty _        = False
canHandBeat (Add c h) card = canCardBeat c card || canHandBeat h card

-- get the lowest card in the Hand
getLowestCard :: Hand -> Card
getLowestCard (Add c Empty) = c
getLowestCard (Add c h)
    | getRank c <= rank lc = c
    | otherwise = lc
  where
    lc = getLowestCard h

-- get all cards on the hand which can beat a given card
betterCards :: Hand -> Card -> Hand
betterCards Empty _ = Empty
betterCards (Add c h) card
    | canCardBeat c card = Add c (betterCards h card)
    | otherwise = betterCards h card

-----------------------------------------------------------------------------------
-- some playful functions
-- remove an element circularly from a list and place the remaining
remove :: [a] -> [[a]]
remove []     = [[]]
remove [_]    = [[]]
remove (x:xs) = xs : map (x :) (remove xs)

-- λ> mapM_ print $ remove [1,2,3,4]
-- [2,3,4]
-- [1,3,4]
-- [1,2,4]
-- [1,2,3]
-- insert an element non-deteministically in any place of a list
ndinsert :: a -> [a] -> [[a]]
ndinsert x []     = [[x]]
ndinsert x (y:ys) = (x : y : ys) : map (y :) (ndinsert x ys)

-- λ> ndinsert 'a' "123"
-- ["a123","1a23","12a3","123a"]
-- division by subtraction
divSub :: (Integral a) => a -> a -> (a, a)
divSub num den = loop num den 0
  where
    loop x y acc
        | x < y = (acc, x)
        | otherwise = loop (x - y) y (acc + 1)

-- concat the numbers of a list
concatNums :: [Int] -> [[Int]]
concatNums []  = [[]]
concatNums [x] = [[x]]
concatNums (x:y:ys) =
    map (x :) (concatNums (y : ys)) ++ concatNums ((x * 10 + y) : ys)

-- λ> mapM_ print $ concatNums [1,2,3]
-- [1,2,3]
-- [1,23]
-- [12,3]
-- [123]
-----------------------------------------------------------------------------------
--
-- implementation of foldr function
-- This is right associative. In the  case of lists when foldr is applied
-- to a binary  operator, a starting value  (typically the right-identity
-- of  the  operator) and  a  list  will  reduce  the list  using  binary
-- operator, from right to left:
--
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc xs =
    case xs of
        []     -> acc
        (y:ys) -> f y (foldr f acc ys)

-- visualize foldr
showFoldr :: Int -> String
showFoldr n =
    foldr ((\x y -> concat ["( ", x, " + ", y, " )"]) . show) "0" [1 .. n]

-- λ> showFoldr 5
-- "( 1 + ( 2 + ( 3 + ( 4 + ( 5 + 0 ) ) ) ) )"
-- Implementation of foldl function
-- This is left  associative. In the case of lists  when foldl is applied
-- to a binary operator, a starting value (typically the left-identity of
-- the  operator), and  a  list will  reduce the  list  using the  binary
-- operator from left to right:
--
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
--
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f acc xs =
    case xs of
        []     -> acc
        (y:ys) -> foldl f (f acc y) ys

-- visualize foldl
showFoldl :: Int -> String
showFoldl n =
    foldl (\x y -> concat ["( ", x, " + ", y, " )"]) "0" $ map show [1 .. n]

-- λ> showFoldl 5
-- "( ( ( ( ( 0 + 1 ) + 2 ) + 3 ) + 4 ) + 5 )"

-----------------------------------------------------------------------------------
-- Towers of Hanoi
-- To move n discs from peg A to peg B:
--   1. move n−1 discs from A to C. This leaves disc #n alone on peg A
--   2. move disc #n from A to B
--   3. move n−1 discs from C to B so they sit on disc #n
data Peg = A
         | B
         | C

instance Show Peg where
  show A = "Peg A"
  show B = "Peg B"
  show C = "Peg C"

instance Eq Peg where
  A == A = True
  B == B = True
  C == C = True
  _ == _ = False

move :: Int -> Peg -> Peg -> Peg -> [String]
move 0 _ _ _ = []
move n src dst aux = moveToAux ++ [moveLastDisc] ++ moveToDst
  where
    moveLastDisc = show src ++ " ==> " ++ show dst
    moveToAux = move (n - 1) src aux dst
    moveToDst = move (n - 1) aux dst src

hanoi :: [String] -> IO ()
hanoi xs = mapM_ printL $ zip [1 .. ] xs
  where
    printL (n, p) = putStrLn $ show n ++ ". " ++ p

-- print the movement of pegs
printMoves :: IO ()
printMoves = do
  putStrLn "Enter the number of discs to be moved from A to C: "
  num <- getLine
  let input = read num :: Int
      moves = move input A C B
  hanoi moves

-- λ> printMoves
-- Enter the number of discs to be moved from A to C:
-- 3
-- 1. Peg A ==> Peg C
-- 2. Peg A ==> Peg B
-- 3. Peg C ==> Peg B
-- 4. Peg A ==> Peg C
-- 5. Peg B ==> Peg A
-- 6. Peg B ==> Peg C
-- 7. Peg A ==> Peg C

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)


-- a task handler
dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("purge", purge)]

-- a function to add a TODO task
add :: [String] -> IO ()
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")

-- a function to list all the TODO tasks
view :: [String] -> IO ()
view [filename] = do
  contents <- readFile filename
  let todoTasks = L.lines contents
      numberedTasks = zipWith (\x line -> show x ++ " - " ++ line) [0 .. ] todoTasks
  putStrLn (L.unlines numberedTasks)

-- a function to delete a task
purge :: [String] -> IO ()
purge [filename, numberedString] = do
  handle <- openFile filename ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberedString
      todoTasks = L.lines contents
      newTodoItems = L.delete (todoTasks !! number) todoTasks
  hPutStr tempHandle (L.unlines newTodoItems)
  hClose handle
  hClose tempHandle
  removeFile filename
  removeFile tempName

main :: IO ()
main = do
  (cmd : args) <- getArgs
  let (Just action) = L.lookup cmd dispatch
  action args

-- guess a random number
guess :: IO ()
guess = do
  gen <- getStdGen
  let (randNum, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range 1 to 10 am I guessing of ?"
  numStr <- getLine
  unless (null numStr) $ do
    let num = read numStr
    if randNum == num
      then putStrLn "Your guess is right!"
      else putStrLn $ "Sorry the number is " ++ show randNum
    _ <- newStdGen
    guess
