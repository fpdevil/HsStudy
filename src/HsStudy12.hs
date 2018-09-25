-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy12
-- Copyright   :
-- License     :
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 12
-- ghc --make -Wall HsStudy12.hs
-- Logical Puzzles from various sources
-----------------------------------------------------------------------------

module HsStudy12
  (
    fastFib
  , pset
  , initialState
  , finalState
  , crossRiver
  , crossesWith
  , isValid
  , moveFarmer
  , moveFox
  , moveGoose
  , moveBeans
  , getValidMoves
  , getMoves
  , recursiveMoves
  , getSolutions
  , solve
  , characters
  , unsafeCombinations
  , isSafe
  , move
  , filterMoves
  , nextState
  , filterPath
  , extend
  , run
  )
where

import qualified Data.List as L


-- A faster Fibonacci
fastFib :: Integer -> Integer
fastFib = fst . fibPair

fibNext :: (Num a) => (a, a) -> (a, a)
fibNext (a, b) = (b, a + b)

fibPair :: (Ord a, Num a) => a -> (a, a)
fibPair n
  | n < 0 = error "Negative numbers not Allowed"
  | n == 0 = (1, 1)
  | n > 0 = fibNext . fibPair $ (n - 1)

-- λ> map fastFib [0..10]
-- [1,1,2,3,5,8,13,21,34,55,89]


-- powerset definition
pset :: [a] -> [[a]]
pset []       = [[]]
pset (x : xs) = pset xs >>= \set -> [set, x : set]

-- λ> pset [1,2,3]
-- [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

-----------------------------------------------------------------------------
-- Fox, goose and bag of beans puzzle
-- https://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle

-- The fox, goose and bag of beans  puzzle is a river crossing puzzle. It
-- dates back to at least the 9th century. Once upon a time a farmer went
-- to a market and  purchased a fox, a goose, and a bag  of beans. On his
-- way home, the  farmer came to the  bank of a river and  rented a boat.
-- But crossing  the river by boat,  the farmer could carry  only himself
-- and a single one  of his purchases: the fox, the goose,  or the bag of
-- beans.

-- If left unattended together, the fox would eat the goose, or the goose
-- would eat the beans.

-- The farmer's challenge  was to carry himself and his  purchases to the
-- far bank of the river, leaving each purchase intact. How did he do it?

-- The first  step must  be to take  the goose across  the river,  as any
-- other will  result in  the goose  or the beans  position eaten.  When the
-- farmer  returns to  the original  side, he  has the  choice of  taking
-- either the fox or  the beans across next. If he  takes the fox across,
-- he would have to return to get  the beans, resulting in the fox eating
-- the goose. If he takes the beans across second, he will need to return
-- to get the fox,  resulting in the beans position eaten  by the goose. The
-- dilemma is solved  by taking the fox (or the  beans) over and bringing
-- the goose  back. Now  he can  take the  beans (or  the fox)  over, and
-- finally return to fetch the goose.

-- His actions in the solution are summarised in the following steps:

--     1. Take the Goose over
--     2. Return
--     3. Take the beans over
--     4. Return with the goose
--     5. Take the fox over
--     6. Return
--     7. Take goose over
-- Thus there are seven crossings, four forward and three back.
-----------------------------------------------------------------------------
-- Represent the initial and final sides as Left and Right
data Side = L | R deriving (Eq)

instance Show Side where
  show L = "left"
  show R = "right"

data Position = Position
            { farmer :: Side
            , fox    :: Side
            , goose  :: Side
            , beans  :: Side
            } deriving (Show, Eq)


-- One Character can be moved to the right bank, and another good can be
-- brought back to the left during each turn. To win, the player must move all
-- Characters from the left bank to the right. However the Fox cannot be left
-- with the Goose, nor the Goose with the Beans otherwise the player loses.

-- define a function for depicting the river crossing
-- river crossing can be from L to R or R to L
crossRiver :: Side -> Side
crossRiver L = R
crossRiver R = L

-- function for checking if one character crosses with another
crossesWith :: (Eq a) => a -> a -> Bool
crossesWith x y = x == y

-- function to check if a particular position for all the four
-- characters is valid or not
isValid :: Position -> Bool
isValid Position {farmer = f, fox = x, goose = g, beans = b} =
  (f == g) || (x /= g && g /= b)

-- functions to move the individual beings from one side to other
-- farmer can cross the river alone or with any being
moveFarmer :: Position -> [Position]
moveFarmer position@Position {farmer = f, fox = _, goose = _, beans = _} =
  [position {farmer = crossRiver f}]

-- fox can cross with a farmer
moveFox :: Position -> [Position]
moveFox position@Position {farmer = f, fox = x, goose = _, beans = _}
  | f `crossesWith` x = [position {farmer = crossRiver f, fox = crossRiver x}]
  | otherwise = []

-- goose can cross with a farmer
moveGoose :: Position -> [Position]
moveGoose position@Position {farmer = f, fox = _, goose = g, beans = _}
  | f `crossesWith` g = [position {farmer = crossRiver f, goose = crossRiver g}]
  | otherwise = []

-- beans can cross with a farmer
moveBeans :: Position -> [Position]
moveBeans position@Position {farmer = f, fox = _, goose = _, beans = b}
  | f `crossesWith` b = [position {farmer = crossRiver f, beans = crossRiver b}]
  | otherwise = []

-- Starting with the initial position of L L L L, the function
-- will check the next valid move and provide the next state or
-- valid position so that all the beings are safe
getValidMoves :: Position -> [Position]
getValidMoves position@Position {farmer = _, fox = _, goose = _, beans = _} =
  filter isValid $ moveFox position ++
                   moveGoose position ++
                   moveBeans position ++
                   moveFarmer position

{-
  test run with initial Position
  λ> getValidMoves Position {farmer = L, fox = L, goose = L, beans = L}
  [Position {farmer = R, fox = L, goose = R, beans = L}]
  this indicates the first valid move from initial state is to move
  the farmer and goose from Left to Right. We can get the next valid
  move or next state in a more monadic way using the bind operator
  λ> getValidMoves Position {farmer = L, fox = L, goose = L, beans = L} >>= getValidMoves
  [Position {farmer = L, fox = L, goose = L, beans = L},
  Position {farmer = L, fox = L, goose = R, beans = L}]
-}

-- get valid moves from a position list without repetition
getMoves :: [Position] -> [Position]
getMoves pos = filter (`notElem` pos) (getValidMoves . head $ pos)

-- recursively get the next valid states from each previous state
recursiveMoves :: [[Position]] -> Position -> [[Position]]
recursiveMoves pos final = do
  ps <- pos
  let moves = getMoves ps
  if final `elem` moves
    then return $ reverse (final : ps)
    else recursiveMoves (map (: ps) moves) final

-- get all the solutions for transforming from an initial state to final
getSolutions :: Position -> Position -> [Position]
getSolutions from to = head $ recursiveMoves [[from]] to

-- finally solve the puzzle
solve :: IO ()
solve = mapM_ print solutions where
  solutions = getSolutions (fillPositions L) (fillPositions R)
  fillPositions p = Position {farmer = p, fox = p, goose = p, beans = p}

-- |
--  Using State
data Character = Farmer
               | Fox
               | Goose
               | Beans
               deriving (Eq, Show)

data State = State Side Side Side deriving (Eq, Show)

type Path = [State]

initialState :: State
initialState = State L L L

finalState :: State
finalState = State R R R

characters :: [Character]
characters = [Farmer, Fox, Goose, Beans]

unsafeCombinations :: [(Character, Character)]
unsafeCombinations = [(Fox, Goose), (Goose, Beans), (Goose, Fox), (Beans, Goose)]

isSafe :: Character -> State -> Bool
isSafe Goose _             = True
isSafe Fox (State _ g b)   = g /= b
isSafe Beans (State f g _) = f /= g

move :: Character -> State -> State
move Fox (State f g b)   = State (crossRiver f) g b
move Goose (State f g b) = State f (crossRiver g) b
move Beans (State f g b) = State f g (crossRiver b)

filterMoves :: State -> [Character]
filterMoves st = filter (`isSafe` st) [Fox, Goose, Beans]

nextState :: State -> [State]
nextState st = map (`move` st) $ filterMoves st

filterPath :: Path -> Path
filterPath p = filter (`notElem` p) (nextState $ last p)

extend :: Path -> [Path]
extend p = map (\s -> p ++ [s]) (filterPath p)

run :: [Path] -> Path
run ps = case L.find ((finalState ==) . last) ps of
  Nothing -> run $ concatMap extend ps
  Just p  -> p

-- λ> mapM_ print $ run [[initialState]]
