module Main (main) where

import Game (initialGame)
import Logic (transformGame)
import Rendering (gameAsPicture)

import Graphics.Gloss

window :: Display
window = InWindow "Tic Tac Toe" (640, 480) (100, 100)

backgroundColor :: Color
backgroundColor = makeColor 255 255 255 255

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (\_ -> id)
