import Data.Void

myvoid :: Void
myvoid = undefined

vf :: IO Void
vf = do
  print "Hi!"
  line <- getLine
  vf

main :: IO ()
main = do
  vf
  return ()