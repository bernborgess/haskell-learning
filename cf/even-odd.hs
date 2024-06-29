-- main=interact$(\x->last$"Mahmoud":["Ehab"|x`div`2==0]).read

-- main=interact$s.read
-- s x=if x`div`2==0 then "Mahmoud" else "Ehab"

-- main=interact$s.read s x=last$"Mahmoud":["Ehab"|x`div`2==0]
-- main=interact$s.read s x=last$"Mahmoud":["Ehab"|even x]

main=interact$s.read
s x|even x="Mahmoud"|1<2="Ehab"
