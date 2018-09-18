-----------------------------------------------------------------------------
-- |
-- Module      :  HsStudy9
-- Copyright   :
-- License     :
--
-- Author      :  Sampath Singamsetty
-- Maintainer  :
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Examples and Exercises in Haskell 9
-- ghc --make -Wall HsStudy9.hs
--
-----------------------------------------------------------------------------
module HsStudy9
  (
    fibIter
  , primes
  , sequenceDo
  , sequenceDo_
  , xfoldM
  , allCombinations
  , sheepFamily
  , parentM
  , parentL
  , paternalGrandFather
  , paternalGrandMother
  , maternalGrandFather
  , maternalGrandMother
  , toMonad
  , parent
  , grandparent
  , traceSheep
  , traceFamily
  , paternalGrandMotherT
  , paternalGrandFatherT
  , maternalGrandFatherT
  , maternalGrandMotherT
  , mothersPaternalGreatGrandFatherT
  , fathersMaternalGreatGrandMotherT
  , parseHexDigit
  , parseDigit
  , parseWord
  , parse
  , parseStr
  ) where

import           Control.Monad
import qualified Data.Char     as C
import           Data.Maybe    (maybeToList)


-- | An iterative version of Fibonacci series
fibIter :: Integer -> Integer
fibIter = iter 1 1
  where
    iter a b 0 = b
    iter a b n = iter (b + a) a (n - 1)

-- | Sieve of Eratosthenes
primes :: [Integer]
primes = sieve [2 ..]

sieve :: (Integral a) => [a] -> [a]
sieve []       = []
sieve (p : ps) = p : sieve (filter (\x -> x `rem` p /= 0) ps)

-- | some standard haskell functions redefined  .. sequence
--   this function  takes a list of  wrapped computations, executes each
--   one in turn and then returns a list of results using do notation
sequenceDo :: (Monad m) => [m a] -> m [a]
sequenceDo [] = return []
sequenceDo (x : xs) = do
  y <- x
  ys <- sequenceDo xs
  return (y : ys)

-- λ> sequenceDo [print 1, print 2, print 3]
-- 1
-- 2
-- 3
-- [(),(),()]

-- | sequenceDo_
--   this function has same behaviour as sequence, but it does not
--   return the list of results. useful only when side effects of
--   monadic computing are important
sequenceDo_ :: (Monad m) => [m a] -> m ()
sequenceDo_ = foldr (>>) (return ())

-- λ> sequenceDo_ [print 1, print 2, print 3]
-- 1
-- 2
-- 3

-- | foldM  is a  monadic version  of foldl  in which  monadic computations
--   built from a list are bound left-to-right.
xfoldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
xfoldM f x []       = return x
xfoldM f x (y : ys) = f x y >>= \z -> xfoldM f z ys

-- λ> xfoldM (\x y ->
--                 putStrLn (show x ++ " op " ++ show y ++ " = " ++ show (x + y))
--            >> return (x + y))
--    0 [1 .. 5]
--
-- 0 op 1 = 1
-- 1 op 2 = 3
-- 3 op 3 = 6
-- 6 op 4 = 10
-- 10 op 5 = 15
-- 15

-- | return a list containing the result of folding the binary operator through
--   all combinations of elements of given lists
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations _ []       = []
allCombinations f (x : xs) = foldl (liftM2 f) x xs

-- | Sheep Cloning Experiment
--   Sheep have parents; most have two, but cloned sheep (e.g., Dolly) have
--   only one,  and the first sheep  (called Adam and Eve  in this example)
--   have no parents (or at least their parents were not sheep!)
data Sheep = Sheep
            { name   :: String
            , mother :: Maybe Sheep
            , father :: Maybe Sheep
            } deriving Show

-- | Build the Sheep family tree for Dolly
sheepFamily :: Sheep
sheepFamily = let adam   = Sheep "Adam" Nothing Nothing
                  eve    = Sheep "Eve" Nothing Nothing
                  uranus = Sheep "Uranus" Nothing Nothing
                  gaea   = Sheep "Gaea" Nothing Nothing
                  holly  = Sheep "Holly" (Just eve) (Just adam)
                  kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                  roger  = Sheep "Roger" (Just eve) (Just kronos)
                  molly  = Sheep "Molly" (Just holly) (Just roger)
              in
                Sheep "Dolly" (Just molly) Nothing

-- The mplus  operator is  used to combine  monadic values  from separate
-- computations into  a single monadic  value. Within the context  of our
-- sheep-cloning  example,  we  could  use  Maybe's  mplus  to  define  a
-- function, parent  sh =  (mother sh) `mplus`  (father sh),  which would
-- return a  parent if  there is  one, and  Nothing is  the sheep  has no
-- parents at  all. For  a sheep  with both  parents, the  function would
-- return one or the other, depending on the exact definition of mplus in
-- the Maybe monad


-- | Parent of a sheep can be either Father or Mother or Nothing
--   interpreted using a Monad
parentM :: Sheep -> Maybe Sheep
parentM sh = father sh `mplus` mother sh

--   interpreted using a list
parentL :: Sheep -> [Sheep]
parentL sh = maybeToList (father sh) `mplus` maybeToList (mother sh)

-- | generic functions for parents and grand parents using Maybe

-- paternal grand father and grand mother
paternalGrandFather :: Sheep -> Maybe Sheep
paternalGrandFather sh = case father sh of
                           Nothing -> Nothing
                           Just f  -> father f

paternalGrandMother :: Sheep -> Maybe Sheep
paternalGrandMother sh = case father sh of
                           Nothing -> Nothing
                           Just f  -> mother f

-- maternal grand father and gand mother
maternalGrandFather :: Sheep -> Maybe Sheep
maternalGrandFather sh = mother sh >>= father
-- maternalGrandFather sh = case mother sh of
--                            Nothing -> Nothing
--                            Just m  -> mother m


maternalGrandMother :: Sheep -> Maybe Sheep
maternalGrandMother sh = mother sh >>= mother
-- maternalGrandMother sh = case mother sh of
--                            Nothing -> Nothing
--                            Just m  -> father m


-- | if the type definition of the Sheep is changed using  a MonadPlus
--   then the definition is applicable for both Maybe as well  as List
--   a helper function for converting to Monad
toMonad :: (MonadPlus m) => Maybe a -> m a
toMonad Nothing  = mzero
toMonad (Just x) = return x

-- | get the parent of a sheep using MonadPlus
parent :: (MonadPlus m) => Sheep -> m Sheep
parent sh = toMonad (father sh) `mplus` toMonad (mother sh)



--   grand Parents of Sheep
grandparent :: (MonadPlus m) => Sheep -> m Sheep
grandparent sh = toMonad (paternalGrandFather sh) `mplus`
                 toMonad (paternalGrandMother sh) `mplus`
                 toMonad (maternalGrandFather sh) `mplus`
                 toMonad (maternalGrandMother sh)

-- A function to trace the ancestors  of a Sheep. The function uses foldM
-- to create a simple  way to trace back in the family  tree to any depth
-- and in any pattern
traceSheep :: Sheep -> [Sheep -> Maybe Sheep] -> Maybe Sheep
traceSheep sh ancestors = foldM getParent sh ancestors
  where
    getParent s f = f s

-- The same function may be redefined using flip and Monad
traceFamily :: (Monad m) => Sheep -> [Sheep -> m Sheep] -> m Sheep
traceFamily sh ancestorsList = foldM (flip ($)) sh ancestorsList

-- With the above trace functions, complex queries may easily be derived
paternalGrandMotherT :: Sheep -> Maybe Sheep
paternalGrandMotherT sh = traceSheep sh [father, mother]

paternalGrandFatherT :: Sheep -> Maybe Sheep
paternalGrandFatherT sh = traceSheep sh [father, father]

maternalGrandMotherT :: Sheep -> Maybe Sheep
maternalGrandMotherT sh = traceFamily sh [mother, mother]

maternalGrandFatherT :: Sheep -> Maybe Sheep
maternalGrandFatherT sh = traceFamily sh [mother, father]

mothersPaternalGreatGrandFatherT :: Sheep -> Maybe Sheep
mothersPaternalGreatGrandFatherT sh = traceSheep sh [mother, father, father, father]

fathersMaternalGreatGrandMotherT :: Sheep -> Maybe Sheep
fathersMaternalGreatGrandMotherT sh = traceSheep sh [father, mother, mother, mother]

-- testing
-- λ> let dolly = sheepFamily
-- λ> paternalGrandFatherT dolly
-- Nothing
-- λ> mothersPaternalGreatGrandFatherT dolly
-- Just (Sheep {name = "Uranus", mother = Nothing, father = Nothing})

-- The example below shows just a  small example of parsing data into hex
-- values,  decimal  values  and   words  containing  only  alpha-numeric
-- characters. Note  that hexadecimal digits overlap  both decimal digits
-- and alphanumeric  characters, leading to an  ambiguous grammar. "dead"
-- is both a valid hex value and a  word, for example, and "10" is both a
-- decimal value of 10 and a hex value of 16.
data Parsed = Digit Integer
            | Hex Integer
            | Word String
            deriving Show

-- to add a character to the parsed representation of a hex digit
-- parseHexDigit :: (MonadPlus m) => Parsed -> Char -> m Parsed
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex i) c
  | C.isHexDigit c = return (Hex ((i * 16) + (toInteger (C.digitToInt c))))
  | otherwise = mzero
parseHexDigit _ _ = mzero

-- to add a character to the parsed representation of a decimal digit
-- parseDigit :: (MonadPlus m) => Parsed -> Char -> m Parsed
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit i) c
  | C.isHexDigit c = return (Digit ((i * 10) + toInteger (C.digitToInt c)))
  | otherwise = mzero
parseDigit _ _ = mzero

-- to add a character to the parsed representation of a word
-- parseWord :: (MonadPlus m) => Parsed -> Char -> m Parsed
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word i) c
  | C.isAlpha c = return (Word (i ++ [c]))
  | otherwise = mzero
parseWord _ _ = mzero

-- parse tries to parse the digit as a list of possible parses like a Hex
-- Value, a Decimal Value or a Word
-- parse :: (MonadPlus m) => Parsed -> Char -> m Parsed
parse :: Parsed -> Char -> [Parsed]
parse p c = (parseHexDigit p c) `mplus`
            (parseDigit p c) `mplus`
            (parseWord p c)

-- parse an entire String and return a list of the possible parsed values
-- parseStr :: (MonadPlus m) => String -> m Parsed
parseStr :: String -> [Parsed]
parseStr str = do
  acc <- return (Hex 0) `mplus` return (Digit 0) `mplus` return (Word "")
  foldM parse acc str
