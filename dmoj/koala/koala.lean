open IO String
def r:IO String:=getStdin>>=FS.Stream.getLine
def main:=r>>=println∘(·-1)∘toNat!∘trim
