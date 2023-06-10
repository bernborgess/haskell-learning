module Main where

import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  mc <- getChar
  case mc of
    ' ' -> do
      print "space"
    'q' -> do
      print "q"
    _ -> return ()
