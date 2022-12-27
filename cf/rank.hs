-- import Data.Bool
-- main = interact $ bool "#Black&White" "#Color" . (any (`elem` "CMY"))


main = interact solve
solve s | any (`elem` "CMY") s = "#Color" | _ = "#Black&White"





