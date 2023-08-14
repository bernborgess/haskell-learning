r = "rock"
s = "scissors"
p = "paper"
w = [(r, s), (s, p), (p, r)]
win a b = (a, b) `elem` w
wins a b c = win a b && win a c
z [a, b, c]
    | wins a b c = "F"
    | wins b a c = "M"
    | wins c a b = "S"
    | otherwise = "?"

main = getLine >>= s2 >>= s3 >>= putStrLn . z
s2 s = getLine >>= (\t -> return [s, t])
s3 l = getLine >>= (\s -> return $ l ++ [s])
