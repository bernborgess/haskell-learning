module Logic (transformGame) where

import Game
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Pure.Game

import Data.Array

isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

switchPlayer :: Game -> Game
switchPlayer game =
  case gamePlayer game of
    PlayerX -> game{gamePlayer = PlayerO}
    PlayerO -> game{gamePlayer = PlayerX}

playerWon :: Player -> Board -> Bool
playerWon player board = any isVictoryProj projs
 where
  projs =
    allRowCoords
      ++ allColumnCoords
      ++ allDiagCoords
  allRowCoords = [[(i, j) | j <- idxs] | i <- idxs]
  allColumnCoords = [[(j, i) | j <- idxs] | i <- idxs]
  allDiagCoords = [[(i, i) | i <- idxs], [(i, n - 1 - i) | i <- idxs]]
  idxs = [0 .. n - 1]
  isVictoryProj proj =
    (n ==) $
      length $
        filter (\cell -> cell == Full player) $
          map (board !) proj

countCells :: Cell -> Board -> Int
countCells cell = length . filter (cell ==) . elems

checkGameOver :: Game -> Game
checkGameOver game
  | playerWon PlayerX board =
      game{gameState = GameOver $ Just PlayerX}
  | playerWon PlayerO board =
      game{gameState = GameOver $ Just PlayerO}
  | countCells Empty board == 0 =
      game{gameState = GameOver Nothing}
  | otherwise = game
 where
  board = gameBoard game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
  | isCoordCorrect cellCoord && board ! cellCoord == Empty =
      checkGameOver $
        switchPlayer $
          game{gameBoard = board // [(cellCoord, Full player)]}
  | otherwise = game
 where
  board = gameBoard game
  player = gamePlayer game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) =
  ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
  , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
  )

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  case gameState game of
    Running -> playerTurn game $ mousePosAsCellCoord mousePos
    GameOver _ -> initialGame
transformGame _ game = game
