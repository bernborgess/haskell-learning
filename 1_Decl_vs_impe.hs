-- //? Declarative VS Imperative

-- Haskell

summer [] = 0
summer (x : xs) = x + summer (xs)

-- sum = foldr (+) 0

-- Java
-- int sum(int *arr, int length) {
--   int i, sum = 0;
--   for(i = 0; i<length; i++) {
--     sum += arr[i];
--   }
--   return sum;
-- }

-- //? LAZY vs Strict evaluation
-- hs

func1 :: Int -> Int
func1 x = x + 2

func2 :: Int -> Int
func2 x = x + 2

func3 :: Int -> Int
func3 x = x + 2

func :: Int -> Int
func arg =
  let x = func1 arg
      y = func2 arg
      z = func3 arg
   in if z > 0 then x else y


-- Java
-- int func(int arg) {
--   int x = func1(arg);
--   int y = func2(arg);
--   int z = func3(arg);

--   if(z>0) {
--     return x;
--   } else {
--     return y;
--   }
-- }