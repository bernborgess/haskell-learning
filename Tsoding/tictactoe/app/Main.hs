module Main (main) where

import Game
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Logic
import Rendering

window :: Display
window = InWindow "Tic Tac Toe" (640, 480) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 255 255 255 255

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (\_ -> id)
