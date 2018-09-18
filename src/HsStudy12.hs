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
-- ghc --make -Wall HsStudy10.hs
-- Logical Puzzles
-----------------------------------------------------------------------------

module HsStudy12
  (
    fastFib
  )
where

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
-- other will  result in  the goose  or the beans  being eaten.  When the
-- farmer  returns to  the original  side, he  has the  choice of  taking
-- either the fox or  the beans across next. If he  takes the fox across,
-- he would have to return to get  the beans, resulting in the fox eating
-- the goose. If he takes the beans across second, he will need to return
-- to get the fox,  resulting in the beans being eaten  by the goose. The
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
data Character = Farmer
               | Fox
               | Goose
               | Beans
               deriving (Eq, Show, Enum)

data Direction = Left | Right deriving (Eq, Show)

characters :: [Character]
characters = [Farmer, Fox, Goose, Beans]

unsafeCombinations :: [(Character, Character)]
unsafeCombinations = [(Fox, Goose), (Goose, Beans)]
