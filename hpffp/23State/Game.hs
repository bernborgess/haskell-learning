module Game where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import System.Exit (exitSuccess)

type GameState = StateT (Int, Int) IO

playGame :: IO ()
playGame = do
    putStrLn "game :)"
    finalState <- execStateT gameLoop (0, 0)
    putStrLn $ "Final: " ++ show finalState

gameLoop :: GameState ()
gameLoop = forever $ do
    liftIO $ putStrLn "$> "
    input <- liftIO getLine
    case input of
        "h" -> move (-1, 0)
        "j" -> move (0, -1)
        "k" -> move (0, 1)
        "l" -> move (1, 0)
        "q" -> liftIO $ putStrLn "Quit" >> liftIO exitSuccess
        _ -> liftIO $ putStrLn "I"

move :: (Int, Int) -> GameState ()
move (dx, dy) = do
    (x, y) <- get
    let ns = (x + dx, y + dy)
    put ns
    liftIO $ putStrLn $ "New state: (" ++ show ns ++ ")"
