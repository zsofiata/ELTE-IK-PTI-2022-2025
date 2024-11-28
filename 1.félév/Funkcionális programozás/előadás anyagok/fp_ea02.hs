-- f :: Double -> Double -> Double
-- f :: Float -> Float -> Float
-- f :: Farcational a => a -> a -> a

--g'  :: Int -> Int -> Int
--g'' :: Integer -> Integer -> Integer 
--g :: Integral a => a  -> a -> a

--iSqrt :: Int -> Int 
--iiSqrt :: Integer -> Integer
iSqrt :: Integral a => a -> a
iSqrt = round . sqrt . fromIntegral

add :: Int -> (Int -> Int)
add x = (+) x 

inc' = (+) 1

-- iSqrt 5 -> (round . sqrt . fromIntegral) 5
-- h (f (g x)) -> (h o f o g) x

--f :: a -> a




