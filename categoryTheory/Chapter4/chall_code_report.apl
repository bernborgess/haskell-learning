
⍝ 1. Construct the Kleisli category for partial functions (define composition and identity).
  nullopt       ← 0 ⍬
  has_value     ← ⊃
  value         ← ⊃⌽
  make_optional ← 1,⊢
      
⍝ 2. Implement the embellished function safe_reciprocal that returns a valid reciprocal of its argument, if it’s different from zero.

  safe_root       ← { ⍵≥0 : make_optional ⍵*.5  ⋄ nullopt }
  safe_reciprocal ← { ⍵≠0 : make_optional ÷⍵    ⋄ nullopt }
      
⍝ 3. Compose safe_root and safe_reciprocal to implement safe_root_reciprocal that calculates sqrt(1/x) whenever possible.

  safe_root_reciprocal ← {
    r ← safe_reciprocal ⍵
    has_value r : safe_root value r ⋄ nullopt
  }