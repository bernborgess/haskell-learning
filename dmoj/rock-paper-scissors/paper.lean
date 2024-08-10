--
--    author:  bernborgess
--    problem: paper - haskellLearning
--    created: 09.August.2024 17:47:24
--

open IO

def getLine : IO String := do return (←(←getStdin).getLine).trim

def getList : IO (List String) := return ((←getLine).split Char.isWhitespace)

def wins : String → String → Bool
| "rock","scissors" => True
| "scissors","paper" => True
| "paper","rock" => True
| _,_ => False

def fn :  (Int × Int) → (String×String) → (Int × Int)
| (a,b),(r,s) =>
  let da := wins r s |>.toNat
  let db := wins s r |>.toNat
  (a + da,b + db)

def main : IO Unit := do
  _ ← getLine
  let as ← getList
  let bs ← getList
  let (a,b) := List.foldl fn (0,0) (as.zip bs)
  let stdout ← getStdout
  stdout.putStrLn s!"{a} {b}"
