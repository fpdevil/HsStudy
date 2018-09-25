{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy4
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    : State Monad examples
-- Evaluation of code modifies the State
-- In imperative style - change value in a variable
-- In functional style - create a new variable with a new value
-----------------------------------------------------------------------------
module HsStudy4 (
                testState,
                spush,
                spop,
                stackOps,
                inc,
                incBy,
                testinc,
                fibo,
                incrementLabel,
                genLabelPair,
                generateLabels,
                runLabels,
                xEnter,
                xAdd,
                xSub,
                xMul,
                xDiv,
                perform,
                testRPNDirect,
                xSwap,
                xDup,
                xSqrt,
                xSin,
                xCos,
                xTan,
                square,
                hypotenuse
                ) where

import           Control.Monad.State

-- a simple test for the state Monad
testState :: Int -> (String, Int)
testState = runState fun
            where
              fun = get >>= \x -> put (x * 8) >> return ['a' .. 'z']

-- a simple stack implementation
type Stack = [Int]

spush :: Int -> State Stack ()
spush x = state (\y -> ((), x : y))

spop :: State Stack Int
spop = state (\(x : xs) -> (x, xs))

stackOps :: State Stack Int
stackOps = spop >>= \x -> spush 5 >> spush 10 >> return x

-- λ> runState stackOps [1,2,3,4,5]
--     (1, [10, 5, 2, 3, 4, 5])
-- λ> runState stackOps [1,2,3,4]
--     (1, [10, 5, 2, 3, 4])
-- λ> testState 5
--     ("abcdefghijklmnopqrstuvwxyz", 40)


-- incrementing with state
inc :: State Int Int
inc = get >>= \x -> put (x + 1) >> return x

-- increment by a value
incBy :: Int -> State Int Int
incBy n = get >>= \_ -> modify (+ n) >> return n

-- testing increment functions
testinc :: IO ()
testinc = do
  print $ evalState inc 12
  print $ execState inc 2
  print $ runState inc 7
  print $ runState (withState (+ 5) inc) 1
  print $ runState (incBy 4) 6
  print $ runState (mapState (\(a, s) -> (a + 3, s + 8)) inc) 1

-- λ> testinc
-- 12
-- 3
-- (7,8)
-- (6,7)
-- (4,10)
-- (4,10)

-----------------------------------------------------------------------------------
-- fibonacci numbers with stte
-----------------------------------------------------------------------------------
fibo :: Int -> State Int Int
fibo 0 = return 1
fibo 1 = return 1
fibo n = do
  x <- fibo (n - 1)
  y <- fibo (n - 2)
  -- modify state to increment by 1 each step
  modify (+ 1)
  return (x + y)

-- λ> runState (fibo 10) 0
--     (89, 88)
-- λ> runState (fibo 10) 9
--     (89, 97)


-----------------------------------------------------------------------------------
-- generating a stream of unique labels
--
-- Let us assume  that we have a requirement of  generating unique labels
-- in code while  performing operations  on an abstract syntax tree (AST)
-- and for this,  each label must be  unique and each label  is needed in
-- various functions. Since within Haskell  it's not possible to mutate the
-- variables   directly,   we   have   to  control   using   the   State.
-----------------------------------------------------------------------------------
-- define a type for holding the State
type Label = State Int

-- now increment the internal state and return a label
incrementLabel :: Label String
incrementLabel = state $ \x -> let y = x + 1
                                  in ("$" ++ show y, y)

-- λ> runState incrementLabel 0
--     ("$1", 1)
-- λ> runState incrementLabel 1
--     ("$2", 2)

-- now generate a pair of labels by lifting the above functions
genLabelPair :: Label (String, String)
genLabelPair = liftM2 (,) incrementLabel incrementLabel

-- λ> runState genLabelPair 0
--     (("$1", "$2"), 2)
-- λ> runState genLabelPair 1
--     (("$2", "$3"), 3)

-- generate the list of label pairs
generateLabels :: Bool -> Label [(String, String)]
generateLabels flag = func <$> genLabelPair
                           <*> genLabelPair
                           <*> genLabelPair
  where
    func a b c = if flag
                 then [a, c]
                 else [a, b, c]

-- λ> runState (generateLabels True) 1
--     ([("$2", "$3"), ("$6", "$7")], 7)
-- λ> runState (generateLabels False) 1
--     ([("$2", "$3"), ("$4", "$5"), ("$6", "$7")], 7)

-- finally run the labels function
runLabels :: IO ()
runLabels = do
  putStrLn "Enter the predicate: `True` or `False`"
  predicate <- getLine
  print $ evalState (generateLabels (read predicate)) 0

-- λ> runLabels
-- Enter the predicate: `True` or `False`
-- True
-- [("$1","$2"),("$5","$6")]
-- λ> runLabels
-- Enter the predicate: `True` or `False`
-- False
-- [("$1","$2"),("$3","$4"),("$5","$6")]

-----------------------------------------------------------------------------------
-- The basic functionality of a hypothetical RPN calculator with infinite stack
-- 1980's calculator emulation using Haskell
-- the calculator will use Reverse Polish Notation (RPN)
-----------------------------------------------------------------------------------
-- define a type for the state of the calculator associated with simple calculation
-- this is intended for internal usage
type ICalState = State [Double]

-- define the same type as above for public usage meant for exporting values
type PCalState = ICalState ()

-- for the proposed stack structure, implement functions/handlers to
-- push and pop values to and from the stack; these may be built upon
-- the standard `get` and `put` from the State monad
push :: Double -> ICalState ()
push p = do
  stk <- get
  put (p : stk)

pop :: ICalState Double
pop = do
  stk <- get
  case stk of
      [] -> return 0.0
      (x : xs) -> do
        put xs
        return x

-- from the calculator's operational point of view, we will export the
-- push function as xEnter
xEnter :: Double -> PCalState
xEnter = push

-- now for some basic arithmetic operations using RPN
binOp :: (Double -> Double -> Double) -> ICalState ()
binOp op = do
  y <- pop
  x <- pop
  push (op x y)
-- do mathematical operations
xAdd, xSub, xMul, xDiv :: PCalState
xAdd = binOp (+)
xSub = binOp (-)
xMul = binOp (*)
xDiv = binOp (/)

                 -- testing the types
                 -- λ> :t (>>)
                 -- (>>) :: Monad m => m a -> m b -> m b
                 -- λ> :t xMul
                 -- xMul :: PCalState
                 -- λ> :t pop
                 -- pop :: ICalState Double
                 -- λ> :t (xMul >> pop)
                 -- (xMul >> pop)
                 --   :: StateT [Double] Data.Functor.Identity.Identity Double
                 -- λ> :t runState
                 -- runState :: State s a -> s -> (a, s)
                 -- λ> :t runState (xMul >> pop)
                 -- runState (xMul >> pop) :: [Double] -> (Double, [Double])
                 -- λ> runState (xMul >> pop) []
                 --     (0.0, [])


--
-- now perform the actual calculations
-- perform ma = fst $ runState (ma >> pop) []
-- or
-- perform ma = head $ snd (runState ma [])
-- This is correct except that the call will fail for an empty stack at which
-- moment the head function will error out. To counter that, an extra pop is
-- used for tacking the error handling


perform :: PCalState -> Double
perform ma = evalState (ma >> pop) []

-- test the RPN calculation (1 + 2) * 3
testRPNDirect :: Double
testRPNDirect = perform $ do
  xEnter 1
  xEnter 2
  xAdd
  xEnter 3
  xMul

-- swap 2 elements
xSwap :: PCalState
xSwap = do
  y <- pop
  x <- pop
  push y
  push x

xDup :: PCalState
xDup = do
  x <- pop
  push x
  push x

-- a unary operation private function
unaryOp :: (Double -> Double) -> ICalState ()
unaryOp op = do
  x <- pop
  push (op x)

-- calculate the square root
xSqrt :: PCalState
xSqrt = unaryOp sqrt

-- trigonometric functions
xSin, xCos, xTan :: PCalState
xSin = unaryOp sin
xCos = unaryOp cos
xTan = unaryOp tan

-- sqaure calculation
square :: PCalState
square = do
  xDup
  xMul

-- calculate hypotenuse
hypotenuse :: PCalState
hypotenuse = do
  square
  xSwap
  square
  xAdd
  xSqrt

{- λ> perform $ xEnter 3 >> xEnter 4 >> hypotenuse
 -     5.0 -}

