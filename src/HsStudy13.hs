-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy13
-- Copyright   :
-- License     :  MIT
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 13
-- ghc --make -Wall HsStudy10.hs
-- Logical Puzzles and others
-----------------------------------------------------------------------------

module HsStudy13
  (
    girls
  , xor
  , isUnique
  , liars
  , rankReceived
  , ranks
  )
where

import           Control.Monad

{-
    SICP #4.4.1 Exercise from Nondeterministic Programs (Liars Puzzle)

Five  schoolgirls sat  for an  examination. Their  parents --  so they
thought --  showed an  undue degree  of interest  in the  result. They
therefore agreed  that, in  writing home  about the  examination, each
girl should make one true statement  and one untrue one. The following
are the relevant passages from their letters:

 # Betty: ‘‘Kitty was second in the examination. I was only third.’’
 # Ethel: ‘‘You’ll be glad to hear that I was on top. Joan was second.’’
 # Joan: ‘‘I was third, and poor old Ethel was bottom.’’
 # Kitty: ‘‘I came out second. Mary was only fourth.’’
 # Mary: ‘‘I was fourth. Top place was taken by Betty.’’

What in fact was the order in which the five girls were placed?
-}

type Name = String
type Rank = Int

data Girl = Girl { girl :: Name
                 , rank :: Rank
                 }

instance Show Girl where
  show g = show (girl g) ++ " got a rank " ++ show (rank g)

girls :: [Name]
girls = ["Betty", "Ethel", "Joan", "Kitty", "Mary"]

-- define a function for Exclusive OR (XOR)
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

-- check if all the elements of a list are distinct or unique
isUnique :: (Ord a) => [a] -> Bool
isUnique []       = True
isUnique [_]      = True
isUnique (x : xs) = (x `notElem` xs) && isUnique xs

-- Since each girl tells exactly one True and one UnTrue statements,
-- only one of their statements can be True at any time. This would
-- also imply that both the statements are True or UnTrue at a time.
liars :: [[Rank]]
liars = do
  betty <- [1 .. 5]
  ethel <- [1 .. 5]
  joan  <- [1 .. 5]
  kitty <- [1 .. 5]
  mary  <- [1 .. 5]
  guard $ isUnique [betty, ethel, joan, kitty, mary]
  guard $ (kitty == 2) `xor` (betty == 3)
  guard $ (ethel == 1) `xor` (joan == 2)
  guard $ (joan == 3) `xor` (ethel == 5)
  guard $ (kitty == 2) `xor` (mary == 4)
  guard $ (mary == 4) `xor` (betty == 1)
  return [betty, ethel, joan, kitty, mary]

rankReceived :: [Girl]
rankReceived = do
  (x, y) <- girls `zip` concat liars
  return Girl {girl = x, rank = y}

ranks :: IO ()
ranks = mapM_ print rankReceived

{-
     Test Execution

λ> ranks
"Betty" got a rank 3
"Ethel" got a rank 5
"Joan" got a rank 2
"Kitty" got a rank 1
"Mary" got a rank 4
-}
