module HCat where

import System.Environment

targetFileName :: IO FilePath
targetFileName = do
  args <- getArgs
  case args of
    [filename] -> pure filename
    _ ->
      ioError $
        userError "please provide a single filename"

runHCat :: IO ()
runHCat = do
  contents <- readFile =<< targetFileName
  putStrLn contents
