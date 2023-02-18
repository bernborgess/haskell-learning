import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State

-- import System.Random

data Direction = N | S | E | W | Quit
  deriving (Read, Show)

type KnightState = (Int, Int)

parseDirection :: String -> Direction
parseDirection s = case s of
  "N" -> N
  "S" -> S
  "E" -> E
  "W" -> W
  "Q" -> Quit

moveKnight :: Direction -> KnightState -> KnightState
moveKnight d (x, y) = case d of
  N -> (x, y + 1)
  S -> (x, y - 1)
  E -> (x + 1, y)
  W -> (x - 1, y)
  Quit -> (x, y)

runGame :: StateT KnightState IO ()
runGame = do
  state <- get
  liftIO $ putStr "Enter a direction: "
  directionStr <- liftIO getLine
  let direction = parseDirection directionStr
  let newState = moveKnight direction state
  put newState
  -- rand <- liftIO $ randomIO :: StateT KnightState IO Double
  let rand = 0.1
  when (rand < 0.2) $ liftIO $ putStrLn "Monster appeared"
  case direction of
    Quit -> return ()
    _ -> do
      liftIO $ putStrLn $ "Knight is at " ++ show newState
      runGame

main :: IO ()
main = do
  putStrLn "Starting game..."
  evalStateT runGame (0, 0)
  putStrLn "Game over."
