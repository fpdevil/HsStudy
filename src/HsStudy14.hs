-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy14
-- Copyright   :
-- License     :  MIT
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 14
-- ghc --make -Wall HsStudy14.hs
-- Logical Puzzles from SICP
-----------------------------------------------------------------------------

module HsStudy14
  (
    fathers
  , daughters
  , isUnique
  , listDaughters
  , solutionSet
  , whosWho
  ) where

import           Control.Monad

{-
SICP #4.4.3 Exercise from Nondeterministic Programs (Daughters Puzzle)

Mary  Ann, Moore’s father has  a yacht  and so  has each  of his  four
friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker.
Each of the  five also has one  daughter and each has  named his yacht
after a  daughter of one  of the others.  Sir Barnacle’s yacht  is the
Gabrielle,  Mr. Moore  owns  the  Lorna; Mr.  Hall  the Rosalind.  The
Melissa,  owned by  Colonel  Downing, is  named  after Sir  Barnacle’s
daughter. Gabrielle’s  father owns the  yacht that is named  after Dr.
Parker’s daughter. Who is Lorna’s father?
-}

type Name = String

data Father = Father { name     :: Name
                     , daughter :: Name
                     , yacht    :: Name
                     }

instance Show Father where
  show f = show (name f) ++ " is the father of "
           ++ show (daughter f) ++ " and his yacht name is "
           ++ show (yacht f)

-- check if all the elements of a list are distinct or unique
isUnique :: (Ord a) => [a] -> Bool
isUnique []       = True
isUnique [_]      = True
isUnique (x : xs) = (x `notElem` xs) && isUnique xs


fathers :: [Name]
fathers = ["Mr.Moore", "Colonel Downing", "Mr.Hall", "Sir Barnacle Hood", "Dr.Parker"]

daughters :: [Name]
daughters = ["Gabrielle", "Lorna", "Melissa", "Rosalind", "Mary"]

listDaughters :: [[(Name, Name, Name)]]
listDaughters = do
  -- set the names of the yachtes of individual persons to daughter's
  -- names as per the question
  let ym = "Lorna"
  let yd = "Melissa"
  let yh = "Rosalind"
  let yb = "Gabrielle"
  let yp = "Mary"

  -- now the daughters of the individuals can be any of the 5 choices
  -- as given in the question, which will be filtered later using the
  -- additional conditions provided
  dm <- daughters
  dd <- daughters
  dh <- daughters
  db <- daughters
  dp <- daughters

  -- as per the puzzle none of the yachtes of the individuals are named
  -- after their own daughters names, so filtering them
  guard (dm /= ym)
  guard (dd /= yd)
  guard (dh /= yh)
  guard (db /= yb)
  guard (dp /= yp)

  -- Mary Ann, Moore's father
  guard (dm == "Mary")
  -- The Melissa, owned by Colonel Downing, is named after Sir Barnacle’s daughter
  guard (db == "Melissa")

  -- now as per the puzzle solved till now,
  -- Mary is the daughter of Moore
  -- Melissa is the daughter of Barnacle
  -- Gabrielle’s father owns the yacht that is named after Dr. Parker’s daughter
  -- so, Gabrielle cannot be the daughter of Parker and can only be the daughter
  -- of either Hall or Downing, in which case the respective yacht's name should
  -- be equal to the daughter of Parker
  guard $ (dd == "Gabrielle" && yd == dp) || (dh == "Gabrielle" && yh == dp)
  -- take only the unique solutions
  guard $ isUnique [dm, dd, dh, db, dp]
  -- return [dm, dd, dh, db, dp]
  return $ zip3 fathers [dm, dd, dh, db, dp] [ym, yd, yh, yb, yp]

-- For rendering the results
whosWho :: [Father]
whosWho = do
  (x, y, z) <- concat listDaughters
  return Father { name = x, daughter = y, yacht = z }

-- final solution set
solutionSet :: IO ()
solutionSet = mapM_ print whosWho

{-
λ> solutionSet
   "Mr.Moore" is the father of "Mary" and his yacht name is "Lorna"
   "Colonel Downing" is the father of "Lorna" and his yacht name is "Melissa"
   "Mr.Hall" is the father of "Gabrielle" and his yacht name is "Rosalind"
   "Sir Barnacle Hood" is the father of "Melissa" and his yacht name is "Gabrielle"
   "Dr.Parker" is the father of "Rosalind" and his yacht name is "Mary"
-}
