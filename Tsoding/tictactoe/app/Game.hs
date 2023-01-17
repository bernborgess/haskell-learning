module Game
  ( n,
    screenWidth,
    screenHeight,
    cellWidth,
    cellHeight,
    initialGame,
    Player (PlayerX, PlayerO),
    Cell (Empty, Full),
    State (Running, GameOver),
    Board,
    Game (gameBoard, gamePlayer, gameState),
  )
where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)

data Cell = Empty | Full Player deriving (Eq, Show)

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game
  { gameBoard :: Board,
    gamePlayer :: Player,
    gameState :: State
  }
  deriving (Eq, Show)

n :: Int
n = 3

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

initialGame :: Game
initialGame =
  Game
    { gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty]),
      gamePlayer = PlayerX,
      -- gameState = Running
      gameState = GameOver (Just PlayerX)
    }
  where
    indexRange = ((0, 0), (2, 2))