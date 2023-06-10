{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Char
import Foreign.C.Types
import System.IO

getHiddenChar = chr . fromEnum <$> c_getch

foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStr "$> "
  mc <- getHiddenChar
  case mc of
    ' ' -> do
      print "space"
    'q' -> do
      print "q"
    _ -> return ()
