def hasN(toks):
    for i,tok in enumerate(toks):
        if tok.isupper():
            return i
    return -1


def solve():
    k = int(input())
    eqs = dict()
    while k>0:
        streq = input()
        if '+' in streq:
            a = streq.split()
            print("NT",a[0],a[2],a[4])
            eqs[a[0]]={"t":"N","a":a[2],"b":a[4]}
        else:
            a = streq.split()
            print("T",a[0],a[2])
            eqs[a[0]]={"t":"T","s":a[2]}

        k -= 1
    for name,oper in eqs.items():
        print(name)
        if oper["t"] == "N":
            print("\t",oper["a"],"+",oper["b"])
        else:
            print("\t",oper["s"])

    toks = ["START"]
    while (i:=hasN(toks))!=-1:
        name = toks[i]
        eq = eqs[name]
        if eq["t"] == "T":
            toks[i] = eq["s"]
        else:
            eqs[name] 



def main():
    T = int(input())
    while T>0:
        solve() 
    T -= 1

main()
