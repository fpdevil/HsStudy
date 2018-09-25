-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy10
-- Copyright   :
-- License     :  MIT
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 10
-- ghc --make -Wall HsStudy10.hs
--
-----------------------------------------------------------------------------

module HsStudy10
  ( isChildStatementValid
  , isParent1StatementValid
  , isParent2StatementValid
  , solve
  , run
  , attacks
  , isLegal
  , queens
  , isSafe
  , boards
  , list2str
  , drawQueens
  , showQueens
  , alternation
  , line
  , triangle
  , sign11
  , sign12
  , solution1
  , sign21
  , sign22
  , solution2
  , sign31
  , sign32
  , solution3
  , sign41
  , sign42
  , solution4
  , sign51
  , sign52
  , solution5
  , sign61
  , sign62
  , solution6
  , sign71
  , sign72
  , solution7
  ) where

import           Control.Monad
import qualified Data.List     as L

-----------------------------------------------------------------------------

-- There is a tribe where all  the Male members speak true statements and
-- Female  members never  speak two  true statements  in a  row, nor  two
-- untrue statements in a row.

-- A researcher  comes across a  mother, a  father, and their  child. The
-- mother and  father speak English  but the  child does not.  However, the
-- researcher asks the child "Are you a boy?". The child responds but the
-- researcher doesn't  understand the response  and turns to  the parents
-- for a translation.

-- Parent 1:  "The child said 'I  am a boy.'"
-- Parent 2:  "The child  is agirl. The  child lied."

-- What is the sex of parent 1, parent 2, the child, and what sex did the
-- child say they were?

data Sex = Male | Female

instance Show Sex where
  show Male   = "Male"
  show Female = "Female"

instance Eq Sex where
  Male == Male     = True
  Female == Female = True
  _ == _           = False

data Solution = Solution
            { parent1          :: Sex    -- ^Sex of Parent1
            , parent2          :: Sex    -- ^Sex of Parent2
            , child            :: Sex    -- ^Sex of Child
            , childDescription :: Sex    -- ^Sex of Parents as per Child
            }

{-
There are 2 axioms
1. A Male does not lie.
2. A Female will never tell two lies or two truths in a row.

and three statements (i.e. logical expressions) in the puzzle:

1. The child said a single statement, in which they declared their sex.
2. Parent 1 said a single statement: "The child said 'I am a a boy'"
3. Parent 2 said two statements: "The child is a girl.  The child lied."

Each of those three statements is realized as a function.  These functions do
not test the truth of the statement but rather test its logical validity in
the face of the axioms.

For example, if the Child is Male then it is not possible the child said they
were Female since that would violate axiom 1.  Similarly if the Child is Female
then no matter if they lied or told the truth the statement is valid in the
face of the axioms, this is an example of the truth of statement differing
from its logical validity.
-}

instance Show Solution where
  show sol = "Parent1 is " ++ show (parent1 sol) ++ "\n" ++
             "Parent2 is " ++ show (parent2 sol) ++ "\n" ++
             "The child is " ++ show (child sol) ++ "\n" ++
             "The child said his parents were " ++ show (childDescription sol) ++ "\n"

{-
         verifying the child's statement
isChildStatementValid(childSex, childDescribedSex)
The only combination that violates  the axioms is (Male, Female) since
a Male does not lie. Obviously  (Male, Male) and (Female, *) are valid
statements.
-}
isChildStatementValid :: Sex -> Sex -> Bool
isChildStatementValid Male Female = False
isChildStatementValid _ _         = True

{-
  verifying parent1's statement
isParent1StatementValid(parent1Sex, childDescribedSex)

Parent  1  said "The  child  said  'I am  a  boy'".  The only  invalid
combination  is  (Male, Female),  because  that'd  imply a  Male  (the
parent) lied. Obviously (Male, Male) is  okay because then parent 1 is
telling the  truth. (Female, *) is  dubious because you can't  trust a
Female.
-}
isParent1StatementValid :: Sex -> Sex -> Bool
isParent1StatementValid Male Female = False
isParent1StatementValid _ _         = True

{-
 verifying parent2's statement
isParent2StatementValid(parent2Sex, childSex, childDescribedSex)

Parent 2 said  "The child is a  girl. The child lied." If  Parent 2 is
Male then the only  way this can be a legal statement  is if the child
is Female  and said they  were Male. This would  mean the child  is in
fact a girl  and the child did  in fact lie, two  statements which are
both true. This corresponds to (Male, Female, Male) being legal.

If Parent2 is Female then (Female, *, Female) are both true.  (Female, Male,
Female) is true because the first statement is false (the child is a girl) but
the second one is true (the child lied -- it said Female when it was Male).
(Female, Female, Female) is also legal since the first statement (the child is
a girl) is true but the second one is a lie (the child lied -- the child said
they were Female and they are Female).

Any other combination will be illegal.
-}
isParent2StatementValid :: Sex -> Sex -> Sex -> Bool
isParent2StatementValid Male Female Male = True
isParent2StatementValid Female _ Female  = True
isParent2StatementValid _ _ _            = False

-- solve the puzzle
-- the gender predicate allows us to check the solutions to
-- the problem based on the input sex og parents as passed
solve :: (Sex -> Sex -> Bool) -> [Solution]
solve gender_predicate = do
  p1 <- [Male, Female]
  p2 <- [Male, Female]
  c  <- [Male, Female]
  cd <- [Male, Female]
  guard $ gender_predicate p1 p2
  guard $ isChildStatementValid c cd
  guard $ isParent1StatementValid p1 cd
  guard $ isParent2StatementValid p2 c cd
  return Solution
    {
      parent1 = p1,
      parent2 = p2,
      child = c,
      childDescription = cd
    }

run :: IO ()
run = do
  putStrLn "##### Heterogeneous Couple #####"
  mapM_ print (solve (/=))
  putStrLn "#####      Gay Couple      #####"
  mapM_ print (solve (\x y -> x == y && x == Male))
  putStrLn "#####    Lesbian Couple    #####"
  mapM_ print (solve (\x y -> x == y && x == Female))

{-
            Sample Execution

λ> run
   ##### Heterogeneous Couple #####
   Parent1 is Female
   Parent2 is Male
   The child is Female
   The child said his parents were Male

#####      Gay Couple      #####
   Parent1 is Male
   Parent2 is Male
   The child is Female
   The child said his parents were Male

#####    Lesbian Couple    #####
   Parent1 is Female
   Parent2 is Female
   The child is Female
   The child said his parents were Female
-}

{-
N-Queens Puzzle

The eight queens  puzzle is the problem of placing  eight chess queens
on an 8×8 chessboard so that  no two queens threaten each other. Thus,
a solution requires that no two  queens share the same row, column, or
diagonal. The eight queens puzzle is  an example of the more general n
queens problem of placing n non-attacking queens on an n×n chessboard,
for which solutions exist for all natural numbers n with the exception
of n=2 and n=3
-}
type Queen = (Int, Int)

-- test if 2 queens are on the same column or same row
isInLine :: Queen -> Queen -> Bool
isInLine (ax, ay) (bx, by) = ax == bx || ay == by

-- test if 2 queens are on the same diagonal (horizonantal distance is
-- same as vertical distance)
isDiagonalTo :: Queen -> Queen -> Bool
isDiagonalTo (ax, ay) (bx, by) = abs (ax - ay) == abs (bx - by)

-- check if 2 queens threaten each other
attacks :: Queen -> Queen -> Bool
attacks q1 q2 = q1 `isInLine` q2 || q1 `isDiagonalTo` q2

-- represent the board as a list of queens
type Board = [Queen]

-- check if the queens position on board is legal
-- This will be the case if the  newly added Queen does not threaten any of
-- the Queens that are already there.

isLegal :: Queen -> Board -> Bool
isLegal queen board = not $ any (queen `attacks`) board

{-
none f l = not . any f $ l

-- the "l" variable is already removable
none f = not . any f

-- prefix notation
none f = (.) not (any f)

-- use of '$' instead of parentheses
none f = (.) not $ any f

-- regrouping (.) and 'not' allows us to move the 'f' further to the end...
none f = (not.) . any $ f

-- and to get rid of it:
none = (not.) . any

-- which implies isLegal = none . attacks
-}

-- solving the Puzzle
-- queens :: (MonadPlus m) => Int -> m Board
queens :: Int -> [Board]
queens n = placeQueens n n

-- place n queens over n rows without attacking
-- placeQueens :: (MonadPlus m) => Int -> Int -> m Board
placeQueens :: Int -> Int -> [Board]
placeQueens _ 0 = return []
placeQueens size row = do
  qs <- placeQueens size (row - 1)
  col <- msum (map return [1 .. size])
  let queen = (row, col)
  guard $ isLegal queen qs
  return (queen : qs)


-- alternate approach
isSafe :: Queen -> [Queen] -> Bool
isSafe _ [] = True
isSafe (x1, y1) ((x2, y2) : rest) =
  if (x1 == x2) || (y1 == y2) || abs (x1 - x2) == abs (y1 - y2) then
    False
  else
    isSafe (x1, y1) rest


-- a generalized solution for exactly 8 Queens
boards :: [(Int, Int)]
boards = do
  q1 <- [(1, i) | i <- [1..8]]
  q2 <- [(2, i) | i <- [1..8]]
  guard $ isSafe q1 [q2]
  q3 <- [(3, i) | i <- [1..8]]
  guard $ isSafe q3 [q2, q1]
  q4 <- [(4, i) | i <- [1..8]]
  guard $ isSafe q4 [q3, q2, q1]
  q5 <- [(5, i) | i <- [1..8]]
  guard $ isSafe q5 [q4, q3, q2, q1]
  q6 <- [(6, i) | i <- [1..8]]
  guard $ isSafe q6 [q5, q4, q3, q2, q1]
  q7 <- [(7, i) | i <- [1..8]]
  guard $ isSafe q7 [q6, q5, q4, q3, q2, q1]
  q8 <- [(8, i) | i <- [1..8]]
  guard $ isSafe q8 [q7, q6, q5, q4, q3, q2, q1]
  [q1, q2, q3, q4, q5, q6, q7, q8]


-- a function to print the integer sequence as a string
list2str :: Int -> String
list2str 0 = ""
list2str n = L.unwords $ map show [1 .. n]


-- a helper function for rendering
drawQueens :: [Int] -> String
drawQueens [] = ""
drawQueens x = list2str size ++ "\n" ++ concatMap showRow x
  where
    size = length x
    spaces n = concat (replicate n "□ ")
    showRow n = spaces (n - 1) ++ "♛ " ++ spaces (size - n) ++ "\n"

showQueens :: Int -> IO ()
showQueens n = mapM_ ((putStrLn . drawQueens) . reverse . map snd) (queens n)

{-
Test Run

λ> showQueens 4
1 2 3 4
□ ♛ □ □
□ □ □ ♛
□ □ ♛ □
♛ □ □ □

1 2 3 4
□ □ ♛ □
□ ♛ □ □
□ □ □ ♛
♛ □ □ □

1 2 3 4
□ □ □ ♛
♛ □ □ □
□ □ ♛ □
□ ♛ □ □

1 2 3 4
□ □ □ ♛
□ ♛ □ □
♛ □ □ □
□ □ ♛ □

-}

-- Triangle
alternation :: Int -> [Int]
alternation start = map (`mod` 2) [start ..]

line :: Int -> String
line x = L.unwords $ map show (take x (alternation x))

triangle :: Int -> IO ()
triangle x = mapM_ (print . line) [1 .. x]

-- λ> triangle 5
-- "1"
-- "0 1"
-- "1 0 1"
-- "0 1 0 1"
-- "1 0 1 0 1"


{-
                     From the Lady or the Tiger

                     The first day

There are two  rooms, and a prisoner has to  choose between them. Each
room contains either a lady or a tiger. In the first test the prisoner
has to choose  between a door with  the sign “In this room  there is a
lady, and in the other room there  is a tiger”, and a second door with
the sign “In one of these rooms there  is a lady and in the other room
there is a  tiger.” A final given  is that one of the  two signs tells
the truth and the other does not.
-}

data Thing = Lady | Tiger deriving (Eq, Show)

-- The First Trial
sign11 :: (Thing, Thing) -> Bool
sign11 (x, y) = x == Lady && y == Tiger

sign12 :: (Thing, Thing) -> Bool
sign12 (x, y) = (x == Lady && y == Tiger) || (x == Tiger && y == Lady)

-- The First Trial says one of the statements is True and other False
solution1 :: [(Thing, Thing)]
solution1 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  -- since only one of the signs is correct
  guard $ sign11 (x, y) /= sign12 (x, y)
  return (x, y)

-- λ> solution1
-- [(Tiger,Lady)]
-- So, the first room had a Lady and second a Tiger
-- The prisoner should choose first room

{-
             The Second Trial
The signs on the doors were changed as below now
Sign1: Atleast one of these rooms contains a Lady
Sign2: A Tiger is in the other room
Either both of them are True or both False
-}
sign21 :: (Thing, Thing) -> Bool
sign21 (x, y) = (x == Lady && y == Tiger) ||
                (x == Tiger && y == Lady) ||
                (x == Lady && y == Lady)

sign22 :: (Thing, Thing) -> Bool
sign22 (x, y) = (x == Tiger && y == Lady) ||
                (x == Tiger && y == Tiger)

solution2 :: [(Thing, Thing)]
solution2 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  -- either both statements are True or both False
  guard $ (sign21 (x, y) && sign22 (x, y)) ||
          (not (sign21 (x, y)) && not (sign22 (x, y)))
  return (x, y)

-- λ> solution2
-- [(Tiger,Lady)]
-- So, Tiger in first room and Lady in second room
-- The prisoner should choose second room

{-
                  The Third Trial

Sign1: Either a Tiger is in this room or a Lady is in the other room
Sign2: A Lady is in the other room
Either both statements are True or both False
-}
sign31 :: (Thing, Thing) -> Bool
sign31 (x, y) = x == Tiger || y == Lady

sign32 :: (Thing, Thing) -> Bool
sign32 (x, y) = (y == Tiger && x == Lady) || (y == Lady && x == Lady)

solution3 :: [(Thing, Thing)]
solution3 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  -- either bot statements are True or both False
  guard $ (sign31 (x, y) && sign32 (x, y)) ||
          (not (sign31 (x, y)) && not (sign32 (x, y)))
  return (x, y)

-- λ> solution3
-- [(Lady,Lady)]
-- This time both rooms contained Ladies
-- The prisoner can choose either room

{-
                 The second day

Well, in each  of the trials of  this day, the king  explained that in
the lefthand room (Room  I), if a lady is in it, then  the sign on the
door is  true, but  if a tiger  is in  it, the sign  is false.  In the
righthand room (Room II), the situation is the opposite: a lady in the
room means  the sign on  the door  is false, and  a tiger in  the room
means the sign is true. Again,  it is possible that both rooms contain
ladies or both rooms contain tigers,  or that one room contains a lady
and the other a tiger.
-}


-- The Fourth Trial
-- Sign1: Both rooms contain Ladies
-- Sign2: Both rooms contain Ladies
-- so, again it's possible that either both are True or both are False
sign41 :: (Thing, Thing) -> Bool
sign41 (x, y) = x == Lady && y == Lady

sign42 :: (Thing, Thing) -> Bool
sign42 (x, y) = x == Lady && y == Lady

solution4 :: [(Thing, Thing)]
solution4 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  guard $ (sign41 (x, y) && sign42 (x, y)) ||
          (not (sign41 (x, y)) && not (sign42 (x, y)))
  -- if both signs are True, then there will be Lady in both rooms
  -- which will be True for sign1 and False for sign2. Hence both
  -- signs should be False
  guard $ x /= Lady
  guard $ y /= Tiger
  return (x, y)

-- λ> solution4
-- [(Tiger,Lady)]
-- Tiger is in Room 1 and Lady is in Room 2
-- Prisoner should choose Room 2

-- The Fifth Trial
-- Sign1: At least one room contains a Lady
-- Sign2: The other room contains a Lady
sign51 :: (Thing, Thing) -> Bool
sign51 (x, y) = (x == Lady && y == Tiger) ||
                (x == Tiger && y == Lady) ||
                (x == Lady && y == Lady)

sign52 :: (Thing, Thing) -> Bool
sign52 (x, y) = (x == Lady && y == Tiger) ||
                (x == Lady && y == Lady)

solution5 :: [(Thing, Thing)]
solution5 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  guard $ (sign51 (x, y) && sign52 (x, y)) ||
          (not (sign51 (x, y)) && not (sign52 (x, y)))
  guard $ x == Lady
  guard $ y == Tiger
  return (x, y)

-- The Sixth Trial
-- Sign1: It makes no difference which room you pick
-- Sign2: There is a Lady in the other Room
sign61 :: (Thing, Thing) -> Bool
sign61 (x, y) = (x == Lady && y == Lady) ||
                (x == Tiger && y == Tiger)

sign62 :: (Thing, Thing) -> Bool
sign62 (x, y) = (x == Lady && y == Tiger) ||
                (x == Lady && y == Lady)

solution6 :: [(Thing, Thing)]
solution6 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  guard $ (sign61(x, y) && sign62(x, y)) ||
          not (sign61 (x, y) && not (sign62 (x, y)))
  -- if Room1 contains a Lady, Room2 should also have it which makes Sign2 False
  -- if Room1 contains a Tiger, then Sign1 is False indicating Room2 will have a
  -- Lady in it; that means Room2 contains a Lady which makes Sign2 False and
  -- Room1 contains a Tiger
  guard $ x /= Lady       -- Sign2 is False
  return (x, y)

-- The Seventh Trial
-- Sign1: It does make a difference which room you pick
-- Sign2: You are better off choosing the other Room
sign71 :: (Thing, Thing) -> Bool
sign71 (x, y) = (x == Lady && y == Tiger) ||
                (x == Tiger && y == Lady)

sign72 :: (Thing, Thing) -> Bool
sign72 (x, y) = x == Lady && y == Tiger

solution7 :: [(Thing, Thing)]
solution7 = do
  x <- [Lady, Tiger]
  y <- [Lady, Tiger]
  guard $ (sign71 (x, y) && sign72 (x, y)) ||
          not (sign71 (x, y)) && not (sign72 (x, y))
  -- if Room1 contains a Lady, sign1 is True and hence Room2 should have a Tiger
  -- if Room1 contains a Tiger then as per Sign1 Room2 should also have a Tiger,
  -- but the Sign1 would be False in such case and Sign2 is True. So for Sign1
  -- to be True and also Sign2 to be also True Room1 should have a Lady and Room2
  -- should have a Tiger
  guard $ x == Lady
  guard $ y == Tiger
  return (x, y)
