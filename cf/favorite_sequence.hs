main=interact$unlines.map(show.maximum.map read.words).u.tail.lines
u[]=[]
u[x]=[x]
u(x:y:w)=y:u(w)


