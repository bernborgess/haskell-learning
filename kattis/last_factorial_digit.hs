main=interact$unlines.map(show.f.read).tail.lines
f x|x==0=1|x>4=0|1>0=f(x-1)*x`mod`10
