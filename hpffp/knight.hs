import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State


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

isMonster :: KnightState -> Bool
isMonster (x, y) = (6 * x + 7 * y - x * x) `mod` 23 == 0

runGame :: StateT KnightState IO ()
runGame = do
  state <- get
  liftIO $ putStr "Enter a direction: "
  directionStr <- liftIO getLine
  let direction = parseDirection directionStr
  let newState = moveKnight direction state
  let monster = isMonster newState
  put newState

  when monster $
    liftIO $
      putStrLn "Monster Appeared!"

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
