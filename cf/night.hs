import Data.Char
main=interact$show.s.(:)0.map((+(-97)).ord).init
s[x]=0
s(x:y:z)=s(y:z)+min(m(x-y))(m(y-x))
m=flip mod 26
