import Data.Bool
main=interact$unlines.map(bool"No""Yes".j.o.map read.words).f.tail.lines
f[]=[]
f[x]=[]
f(x:y:t)=y:f t
o[]=(0,0)
o(x:t)=(a+l,b+r)
 where (a,b)=o t
       r=q.odd$x
       l=q.even$x
q=bool 1 0
j(a,b)=a==b
