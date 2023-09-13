import Control.Monad (when)
import Control.Monad.State (MonadState (state), State, runState)

type Stack = [Int]

pop :: State Stack Int
pop = state fn
  where
    fn [] = (0, [])
    fn (x : xs) = (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

operating :: State Stack Int
operating = do
    push 3
    push 4
    pop
    pop

stackers :: State Stack ()
stackers = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

stackinn :: State Stack ()
stackinn = do
    a <- operating
    when (a == 100) stackers

main = do
    print $ (runState operating) []
    print $ (runState stackers) []
    print $ (runState stackers) []
    print $ (runState stackinn) []

{-
main :: IO ()
main = do
    let initialStack = Stack []

    let fn = undefined $ do
            push 1
            push 2
            push 3
            val1 <- top
            pop
            val2 <- top
            let isEmpty = empty
            return (val1, val2, isEmpty)

    final <- mapM fn initialStack -- :: Int
    print final

-}