{-Bozó István
2.518
bozo_i@inf.elte.hu
10:15
-}

--inc :: Int -> Int 
inc x = x + 1
square x = x * x
squareInc x = square (inc x)

--mohó vs. lusta

-- squareInc 7 ->
-- square (inc 7)
-- square (8)
-- 8 * 8
-- 64

-- squareInc 7 ->
-- square (inc 7) ->
-- (inc 7) * (inc 7)
-- (7+1)   * (inc 7)
-- 8       * (inc 7)
-- ...
-- 8 * 8
-- 64
