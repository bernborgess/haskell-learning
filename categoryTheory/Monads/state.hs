import Control.Monad

import Data.List (foldl', isPrefixOf)

-------------------------------------------------------------------------------
-- State Monad Implementation
-------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State x) = State $ \s ->
    let (x', s') = x s
     in (f x', s')

instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State x) = State $ \s ->
    let (f', s') = f s
        (x', s'') = x s'
     in (f' x', s'')

instance Monad (State s) where
  -- return a = State $ \s -> (a, s)
  return = pure

  State act >>= k = State $ \s ->
    let (a, s') = act s
     in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

evalState :: State s a -> s -> a
evalState act = fst . runState act

execState :: State s a -> s -> s
execState act = snd . runState act

-------------------------------------------------------------------------------
-- IO Code
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Example
-------------------------------------------------------------------------------

type Stack = [Int]

empty :: Stack
empty = []

pop :: State Stack Int
pop = State $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

tos :: State Stack Int
tos = State $ \(x : xs) -> (x, x : xs)

-------------------------------------------------------------------------------
-- User interaction
-------------------------------------------------------------------------------

{-
stackManip :: State Stack Int
stackManip = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a+b)
    tos

main :: IO ()
main = do
  let res = evalState stackManip empty
  print res
-}

-- TODO: each command should bind to the next,
-- instead of being independent!
main :: IO ()
main =
  interact
    ( unlines
        . map
          ( \cmd ->
              show $ evalState (parseCommands cmd) empty
          )
        . lines
    )

{-
main :: IO ()
main = do
  putStrLn "Haskell Stack - Interactive Mode"
  runInputT haskelineSettings interactive
  where haskelineSettings = setComplete autoComplete defaultSettings

type CmdHandler = [String] -> IO ()
type CmdInfo = (String, CmdHandler, String, String)

cmds :: [CmdInfo]
cmds =
    [("push",        push,      "<description>",         "adds a task")
    ]

autoComplete :: CompletionFunc IO
autoComplete (leftStr, _)
        | length (words leftStr) > 1 = return ("", [])
        | otherwise = return ("", completions)
    where
        word = reverse leftStr
        completions  = map simpleCompletion $ filter (word `isPrefixOf`) cmdNames
        cmdNames = map cmdInfoToName cmds
        cmdInfoToName (name,_,_,_) = name
-}

{-
main = forever $ do
  putStr "$> "
  cmd <- getLine
  print $ evalState (parseCommands cmd) empty

  commands <- getLine
  interact $ map (\ cmd ->
    evalState (parseCommands cmd) empty) . lines

  (\inp ->
      mapM_ (\cmd ->
        evalState (parseCommands cmd) empty)
      $ lines inp
    )
  let res = evalState (parseCommands commands) empty
  print res
-}

parseCommands :: String -> State Stack Int
parseCommands commands = do
  let cmds = lines commands
  mapM_ (executeCommand . words) cmds
  tos

executeCommand :: [String] -> State Stack ()
executeCommand cmd = case head cmd of
  "push" -> do
    let a = read $ cmd !! 1 :: Int
    push a
  "pop" -> do
    _ <- pop
    return ()
  "tos" -> do
    _ <- tos
    return ()
  _ -> do return ()