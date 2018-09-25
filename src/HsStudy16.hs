-----------------------------------------------------------------------------
-- |
-- Module         : HsStudy16
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    : A pedagogical implementation of the State monad
--                  Using State Monad
--
-----------------------------------------------------------------------------
module HsStudy16
  ( state
  , get
  , put
  , modify
  , evalState
  , execState
  , mapState
  , cpush
  , cpop
  , uniOp
  , binOp
  , xEnter
  , xAdd
  , xDiv
  , xMul
  , xSub
  , apply
  , perform
  , testC
  , xSwap
  , xSqrt
  , xSquare
  , xSin
  , xCos
  , xTan
  , hypotenuse
  ) where

import           Control.Applicative ()
import           Control.Monad       ()

{-
  STATE Monad

A stateful computation can be represented in a purely-functional way as
a function from states to pairs of result and new state:

          f : state -> {result, state}
-}
-- if s = type of the State of the computation
--    a = type of the produced result
-- then we can define the State interms of a newtype as follows:
--
newtype State s a = State
  { runState :: s -> (a, s)
  }

-- The runState function takes a stateful  computation or a value `a` and
-- an initial  State `s` as  parameters. It unwraps the  computation from
-- the State type and simply applies it to the initial State. In  a sense
-- it is simply the opposite of state.
--
-- with newtype definition, the runState will be as follows
-- runState :: State s a -> s -> (a, s)
--
-- its analgous to runState (State f) s =  f s
-----------------------------------------------------------------------------------
-- define a state constructor
-----------------------------------------------------------------------------------
state :: (s -> (a, s)) -> State s a
state = State

-----------------------------------------------------------------------------------
-- State Monad as an instance of Functor
-----------------------------------------------------------------------------------
instance Functor (State s) where
  fmap f mx =
    State $ \s ->
      let (x, t) = runState mx s
      in (f x, t)

-----------------------------------------------------------------------------------
-- State Monad as an instance of Applicative
-----------------------------------------------------------------------------------
instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  af <*> ax =
    State $ \s ->
      let (f, t) = runState af s
          (x, u) = runState ax t
      in (f x, u)

-----------------------------------------------------------------------------------
-- State Monad as an instance of Monad
-----------------------------------------------------------------------------------
instance Monad (State s) where
  return a = State $ \s -> (a, s)
  mx >>= f =
    State $ \s ->
      let (x, t) = runState mx s
          my = f x
          (y, u) = runState my t
      in (y, u)

-----------------------------------------------------------------------------------
-- internal State extraction through the `get` function; it generates a
-- computation that returns the unchaged state as both the result value
-- and the new State
-----------------------------------------------------------------------------------
get :: State s s
get = State $ \s -> (s, s)

-----------------------------------------------------------------------------------
-- Set's or replaces the State inside the Monadic context using the `put`
-- function; it generates a computation that returns whatever state as the
-- result value and as the new state
-----------------------------------------------------------------------------------
put :: s -> State s ()
put x = State $ const ((), x)

-----------------------------------------------------------------------------------
-- The `modify` function modifies the state (read/write/modify). Its a
-- monadic state transformer that maps an old state to a new state within
-- a State monad. The old State is simply thrown away.
-----------------------------------------------------------------------------------
modify :: (s -> s) -> State s ()
modify f = do
  current <- get
  put (f current)

-----------------------------------------------------------------------------------
-- The function `evalState` will evaluate a stateful computation with the
-- given initial state and returns the final value , discarding final State
-- evalState (State f) s = first (f s)
-----------------------------------------------------------------------------------
evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

-----------------------------------------------------------------------------------
-- The function `execState` will evaluate a stateful computation with the
-- given initial state and returns the final state, discarding final value
-- execState (State f) s = second (f s)
-----------------------------------------------------------------------------------
execState :: State s a -> s -> s
execState ma s = snd (runState ma s)

-----------------------------------------------------------------------------------
-- The function `mapState` maps both the return value and the final state
-- of a computation using the given function
-----------------------------------------------------------------------------------
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

-----------------------------------------------------------------------------------
-- A hypothetical RPN calculator with infinite stack
-----------------------------------------------------------------------------------
-- define a type for the simple calculator
type CalcState = State [Double]

-- define a type for the exported usage
type Calculator = CalcState ()

-- if additional features like store and recall functionality needs to be
-- implemented, include memory location as below
data InternalState = InternalState
                          { stack  :: [Double]
                          , memory :: Double
                          }

-- and redefine the CalcState as below
type CalculatorState = State InternalState

  {- private functions -}

-- Define a  couple of monadic functions  that enable us to  push and pop
-- values off  of the stack. These  may be considered as  a primitives on
-- top of the still more primitive get and put functions
cpop :: CalcState Double
cpop = do
  stk <- get
  case stk of
    [] -> return 0.0
    (x : xs) -> do
      put xs
      return x

cpush :: Double -> CalcState ()
cpush p = do
  stk <- get
  put (p : stk)

-- unary operation
uniOp :: (Double -> Double) -> CalcState ()
uniOp op = do
  x <- cpop
  cpush (op x)

-- binary operation
binOp :: (Double -> Double -> Double) -> CalcState ()
binOp op = do
  y <- cpop
  x <- cpop
  cpush (op x y)

-- export the push operation on a different name
xEnter :: Double -> Calculator
xEnter = cpush

-- common arithmetic functions for Calculator
xAdd, xSub, xDiv, xMul :: Calculator
xAdd = binOp (+)
xSub = binOp (-)
xDiv = binOp (/)
xMul = binOp (*)

-- calculation of the results
apply :: Calculator -> Double
apply mx = fst $ runState (mx >> cpop) []

-- using State monad functions, the above can be
perform :: Calculator -> Double
perform mx = evalState (mx >> cpop) []

-- test the functions
testC :: Double
testC = perform $ do
  xEnter 2
  xEnter 3
  xAdd
  xEnter 1
  xMul

-- swap the input elements
xSwap :: Calculator
xSwap = do
  y <- cpop
  x <- cpop
  cpush y
  cpush x

-- duplication of the input elements
xDup :: Calculator
xDup = do
  x <- cpop
  cpush x
  cpush x

-- square root of an element
xSqrt :: Calculator
xSqrt = uniOp sqrt

-- trigonometric functions
xSin, xCos, xTan :: Calculator
xSin = uniOp sin
xCos = uniOp cos
xTan = uniOp tan

-- do a square of the value
xSquare :: Calculator
xSquare = do
  xDup
  xMul

-- calculate Hypotenuse
hypotenuse :: Calculator
hypotenuse = do
  xSquare
  xSwap
  xSquare
  xAdd
  xSqrt

-- λ> perform $ xEnter 3 >> xEnter 4 >> hypotenuse
-- 5.0
