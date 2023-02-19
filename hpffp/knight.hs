import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)
import System.Process (system)

data Direction = N | S | E | W | Quit
  deriving (Read, Show)

data Weapon = None | Knife | Sword | Bow

instance Show Weapon where
  show None = "x"
  show Knife = "(=-"
  show Sword = "|=="
  show Bow = ")->"

type KnightPosition = (Int, Int)

data GameState = GameState
  { knightPosition :: KnightPosition,
    knightMoney :: Int,
    knightWeapon :: Weapon
  }
  deriving (Show)

parseDirection :: String -> Direction
parseDirection s = case s of
  "N" -> N
  "S" -> S
  "E" -> E
  "W" -> W
  _ -> Quit

moveKnight :: Direction -> KnightPosition -> KnightPosition
moveKnight d (x, y) = case d of
  N -> (x, y + 1)
  S -> (x, y - 1)
  E -> (x + 1, y)
  W -> (x - 1, y)
  Quit -> (x, y)

isMonster :: KnightPosition -> Bool
isMonster (x, y) = (6 * x + 7 * y - x * x) `mod` 23 == 0

showTile :: GameState -> KnightPosition -> Char
showTile state (x, y)
  | knightPosition state == (x, y) = 'H'
  | isMonster (x, y) = 'M'
  | otherwise = ' '

runGame :: StateT GameState IO ()
runGame = do
  state <- get
  liftIO $ putStrLn $ replicate 20 '-'
  liftIO $ putStr "Enter a direction: "
  directionStr <- liftIO getLine
  liftIO $ system "clear"
  let direction = parseDirection directionStr
  let newPosition = moveKnight direction $ knightPosition state
  let hasMonster = isMonster newPosition
  let newState =
        state
          { knightPosition = newPosition,
            knightMoney = knightMoney state + if hasMonster then 5 else 0
          }

  put newState

  when hasMonster $
    liftIO $
      putStrLn "Monster Appeared!"

  let (x, y) = knightPosition newState
  let grid = [[showTile newState (i, j) | i <- [0 .. 9]] | j <- [9, 8 .. 0]]
  liftIO $ mapM_ putStrLn grid

  case direction of
    Quit -> return ()
    _ -> do
      liftIO $
        putStrLn $
          "Knight is at "
            ++ show newPosition
            ++ ", carrying "
            ++ show (knightMoney newState)
            ++ " gold pieces, and wielding "
            ++ show (knightWeapon newState)
      runGame

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Starting game..."
  evalStateT runGame $ GameState (0, 0) 0 None
  putStrLn "Game over."
