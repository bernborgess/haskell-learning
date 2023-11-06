newtype Mu a = Mu (Mu a -> a)

y f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)

fact f n = if n == 0 then 1 else n * f (n - 1)

-- y f =
--     let
--         a x = x x
--         b x =
--             let
--                 c y = x x y
--              in
--                 f c
--      in
--         a b