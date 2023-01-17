module Main (main) where

import Game (initialGame)
import Graphics.Gloss
import Logic (transformGame)
import Rendering (gameAsPicture)

window :: Display
window = InWindow "Tic Tac Toe" (640, 480) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 0

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (\_ -> id)
