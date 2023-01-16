main = interact $ pyramid . read

asterisks = '*' : asterisks

pyramid :: Int -> String
pyramid 0 = ""
pyramid n = pyramid (n-1) ++ take n asterisks ++ "\n"

