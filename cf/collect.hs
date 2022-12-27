main=interact$unlines.map((\b->last$"NO":["YES"|b]).s.map read.words).tail.lines
s l=y>=0&&y`mod`3==0 where y=sum l-3*(maximum.init$l)
