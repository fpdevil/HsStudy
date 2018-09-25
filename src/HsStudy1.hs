{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy1
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    : Natural numbers re-implemented
--
-----------------------------------------------------------------------------
module HsStudy1
    ( Peano(..)
    , nat2int
    , int2nat
    , zero
    , one
    , two
    , three
    , four
    , five
    , six
    , seven
    , eight
    , nine
    , ten
    , isZero
    , isSucc
    , infinity
    , err
    , prec
    , add
    , even
    , odd
    , fact
    , fibonacci
    , freqs
    , chisqr
    , rotate
    , bin2int
    , binary2int
    , int2bin
    , xfoldl
    , xfoldr
    , showFoldr
    , showFoldl
    , evens
    , odds
    ) where

import qualified Data.Char as C
import           Prelude   hiding (even, odd)

-- Peano Arithmetic for Natural numbers 0, 1, 2 ...
-- Peano numbers are  a simple way of representing  natural numbers using
-- only a zero  value and a successor function. In  haskell, it is easier
-- to create the  Peano number values, but since  unary representation is
-- inefficient, they  are more often  useful in type arithmetic  owing to
-- their simplicity.
data Peano
    = Zero -- ^ Zero
    | Succ Peano -- ^ Successor of another Peano number

-- error function
err :: String -> String -> a
err func msg = error ("HsStudy1.Peano." ++ func ++ ": " ++ msg)

-- auxilliary function for negative number subtraction
subNeg :: Peano -> Peano -> (Bool, Peano)
subNeg Zero y            = (False, y)
subNeg x Zero            = (True, x)
subNeg (Succ x) (Succ y) = subNeg x y

-- testing for Zero
isZero :: Peano -> Bool
isZero Zero     = True
isZero (Succ _) = False

-- make Peano an instance of the Num class
instance Num Peano where
    x + Zero = x
    x + (Succ y) = Succ (x + y)
    x - y =
        let (flag, z) = subNeg y x
        in if flag
               then err "subtract" "negative difference for Peano"
               else z
    _ * Zero = Zero
    Zero * _ = Zero
    x * (Succ y) = x * y + x
    fromInteger x
        | x < 0 = err "fromInteger" "for a negative number"
    fromInteger 0 = Zero
    fromInteger x = Succ (fromInteger (x - 1))
    abs = id
    signum Zero     = Zero
    signum (Succ _) = Succ Zero
    negate Zero = Zero
    negate _    = err "negate" "not defined for Peano"

-- instance of the Eq class
instance Eq Peano where
    Zero == Zero = True
    Zero == (Succ _) = False
    (Succ _) == Zero = False
    (Succ x) == (Succ y) = x == y

-- instance of the Ord class
instance Ord Peano where
    Zero `compare` Zero = EQ
    Zero `compare` (Succ _) = LT
    (Succ _) `compare` Zero = GT
    (Succ x) `compare` (Succ y) = x `compare` y
    min (Succ x) (Succ y) = Succ (min x y)
    min _ _               = Zero
    max (Succ x) (Succ y) = Succ (max x y)
    max Zero y            = y
    max x Zero            = x

-- instance of the Real class
instance Real Peano where
    toRational = toRational . toInteger

-- instance of the Integral class
instance Integral Peano where
    quot = div
    rem = mod
    quotRem = divMod
    div x y = fst (divMod x y)
    mod x y = snd (divMod x y)
    divMod x y =
        let (isNeg, d) = subNeg y x
        in if isNeg
               then (Zero, x)
               else let (q, r) = divMod d y
                    in (Succ q, r)
    toInteger Zero     = 0
    toInteger (Succ x) = 1 + toInteger x

-- instance of the Enum class
instance Enum Peano where
    succ = Succ
    pred Zero     = err "pred" "No predecessor for Zero"
    pred (Succ a) = a
    enumFrom = iterate Succ
    toEnum = fromIntegral
    fromEnum = fromIntegral

-- inifinite sequence of Peano numbers
infinity :: Peano
infinity = Succ infinity

count :: Peano -> Int
count Zero     = 0
count (Succ n) = 1 + count n

instance Show Peano where
    show n = "Peano " ++ show (count n)

-- instance of Bounded
instance Bounded Peano where
    minBound = Zero
    maxBound = infinity

nat2int :: Peano -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Peano
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Peano -> Peano -> Peano
add Zero n     = n
add (Succ m) n = Succ (add m n)

-- common names for first 10 natural numbers
zero, one, two, three, four, five, six, seven, eight, nine, ten :: Peano
zero = Zero

one = Succ zero

two = Succ one

three = Succ two

four = Succ three

five = Succ four

six = Succ five

seven = Succ six

eight = Succ seven

nine = Succ eight

ten = Succ nine

isSucc :: Peano -> Bool
isSucc = not . isZero

prec :: Peano -> Peano
prec Zero     = Zero
prec (Succ x) = x

even :: Peano -> Bool
even x
    | y == 0 = True
    | y == 1 = False
    | otherwise = even (x - two)
  where
    y = nat2int x

odd :: Peano -> Bool
odd x
    | y == 0 = False
    | y == 1 = True
    | otherwise = odd (x - one)
  where
    y = nat2int x

-- factorial of numbers
fact :: Peano -> Peano
fact Zero     = Succ Zero
fact (Succ n) = Succ n * fact n

-- efficient fibonacci calculation
fibstep :: (Peano, Peano) -> (Peano, Peano)
fibstep (Zero, Zero)     = (Zero, Zero)
fibstep (Zero, Succ v)   = (Succ v, Succ v)
fibstep (Succ u, Zero)   = (Zero, Succ u)
fibstep (Succ u, Succ v) = (Succ v, Succ u + Succ v)

fibpair :: Peano -> (Peano, Peano)
fibpair n
    | n == Zero = (Zero, Succ Zero)
    | otherwise = fibstep (fibpair (n - 1))

fibonacci :: Peano -> Peano
fibonacci = fst . fibpair

----------------------------------------------------------------------
-- PEANO Numbers end here
----------------------------------------------------------------------
--
-- letter frequencies in a string
percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

-- lower case characters length
lowers :: String -> Int
lowers str = length [c | c <- str, C.isLower c]

-- count the character occurrences in a string
charCount :: Char -> String -> Int
charCount c s = length [x | x <- s, x == c]

-- frequency of occurrence of characters in a string
freqs :: String -> [Float]
freqs str = [percent (charCount c str) n | c <- ['a' .. 'z']]
  where
    n = lowers str

-- chi-square statistic
chisqr :: [Float] -> [Float] -> Float
chisqr observed expected =
    sum [((o - e) ^ 2) / e | (o, e) <- zip observed expected]

-- rotate chars in a string
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

----------------------------------------------------------------------
-- Binary Numbers
----------------------------------------------------------------------
-- Binary Bits
type Bit = Int

-- convert a binary number list to integer
bin2int :: [Bit] -> Int
bin2int bits = sum [p * b | (p, b) <- zip step bits]
  where
    step = iterate (* 2) 1

-- using fold function
binary2int :: [Bit] -> Int
binary2int = foldr (\x y -> x + 2 * y) 0

-- integer to binary
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- re-implement fold functions
-- left fold -> xfoldl
xfoldl :: (a -> b -> a) -> a -> [b] -> a
xfoldl step acc (x:xs) = xfoldl step (step acc x) xs
xfoldl _ acc []        = acc

{- λ> mapM_ print $ scanl (\a b -> concat ["{ ", a, " * ", b, " }"]) "id" (map show ['a' .. 'e'])
 - "id"
 - "{ id * 'a' }"
 - "{ { id * 'a' } * 'b' }"
 - "{ { { id * 'a' } * 'b' } * 'c' }"
 - "{ { { { id * 'a' } * 'b' } * 'c' } * 'd' }"
 - "{ { { { { id * 'a' } * 'b' } * 'c' } * 'd' } * 'e' }" -}
-- right fold -> xfoldr
xfoldr :: (a -> b -> b) -> b -> [a] -> b
xfoldr step acc (x:xs) = step x (xfoldr step acc xs)
xfoldr _ acc []        = acc

{- λ> scanr (\a b -> concat ["{ ", a, " * ", b, " }"]) "id" (map show ['a' .. 'e'])
 -     ["{ 'a' * { 'b' * { 'c' * { 'd' * { 'e' * id } } } } }",
 -      "{ 'b' * { 'c' * { 'd' * { 'e' * id } } } }",
 -      "{ 'c' * { 'd' * { 'e' * id } } }", "{ 'd' * { 'e' * id } }",
 -      "{ 'e' * id }", "id"] -}
-- demos for foldl and foldr
stepper :: String -> String -> String
stepper a b = concat ["{ ", a, " * ", b, " }"]

showFoldr :: Int -> String
showFoldr n = xfoldl stepper "0" (map show [1 .. n])

-- λ> showFoldr 4
--     "{ { { { 0 * 1 } * 2 } * 3 } * 4 }"
showFoldl :: Int -> String
showFoldl n = xfoldr stepper "0" (map show [1 .. n])

-- λ> showFoldl 4
--     "{ 1 * { 2 * { 3 * { 4 * 0 } } } }"
-- mutual recursion
-- select the elements from a list at odd and even positions
evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs
