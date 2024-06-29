-- Ex 1. Define the reader monad. It's supposed to model
-- computations that have access to some read-only
-- environment. In imperative code such environment is
-- often implemented as a global object. In functional
-- languages we need to pass it as an argument to every
-- function that might potentially need access to it. The
-- reader monad hides this process.
{-
newtype Reader e a = Reader (e -> a)

reader :: (e -> a) -> Reader e a
reader = Reader

runReader :: Reader e a -> e -> a
runReader (Reader act) = act

ask :: Reader e e
ask = reader id

instance Monad (Reader e) where
  -- return x = reader (\_ -> x)
  rd >>= k = reader $ \env ->
    let x = runReader rd env
        act' = k x
     in runReader act' env

type Env = Reader String

-- curried version of
-- type Env a = Reader String a
-}

import Control.Monad.State
import Data.Char
import Data.Map qualified as M

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

type SymTab = M.Map String Double

type Evaluator a = State SymTab a

lookUp :: String -> Evaluator Double
lookUp str = do ...
