main=interact$show.(g 1 1).read
g i s n=last$g(i+1)(s+(i^2+3*i+2)`div`2)n:[i-1|s>n]
