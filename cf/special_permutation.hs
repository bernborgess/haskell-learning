main=interact$unlines.map(foldr(\x a->show x++" "++a)"".s.read).tail.lines
s n=[2..n]++[1]