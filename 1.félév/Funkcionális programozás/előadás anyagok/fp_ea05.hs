--import Prelude hiding (min)
isPrime :: Integral a => a -> Bool
--isPrime n = length (divisors n) == 2
isPrime 1 = False
isPrime 2 = True
isPrime n = null (divisors n) 

divisors :: Integral a => a -> [a]
divisors n = [ i | i <- [2..n `div` 2], n `mod` i == 0]

primes :: Integral a => Int -> [a]
primes n = take n [ i | i <- [2..], isPrime i]

-- divisors 12 == [1,12]
-- [1,2]
-- False

isX :: Char -> Bool
isX 'x' = True
isX 'X' = True
isX _   = False

isEmpty :: [a] -> Bool -- null
isEmpty [] = True
isEmpty _  = False

isSingleton :: [a] -> Bool
isSingleton (_:[]) = True
isSingleton _      = False


isDoubleton :: [a] -> Bool
-- isDoubleton (_:l) = isSingleton l
isDoubleton (_:_:[]) = True
isDoubleton _     = False

isNegative :: (Ord a,Num a) => a -> Bool
isNegative n
  | n < 0     = True
  | otherwise = False


min' x y | x <= y = x
min' x y = y

-- 1*2*3*...*n
-- n*(n-1)*... 1
-- n*(n-1)!
--fact n = product [1..n]
fact :: Integer -> Integer
fact 0 = 1
fact n | n > 0 = n * fact (n-1)

-- fact 5 ->
-- 5 * fact 4
--     4 * fact 3
--         3 * fact 2
--             2 * fact 1
--                 1 * fact 0
--                     1
-- ...
-- 120

fact' :: Integer -> Integer
fact' n = factH n 1
 where
 --(x,y) = ...
 factH :: Integer -> Integer -> Integer
 factH 0 c = c
 factH n c = factH (n-1) (c*n)


-- fact' 5 ->
-- factH 5 1 
-- factH 4 (1*5)
-- factH 3 (1*5)*4
-- factH 2 60
-- factH 1 120
-- factH 0 120
-- 120

--data Answer = Yes | Maybe | No
 
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


length'' :: ([a] -> Int)
length'' = lengthH 0 

lengthH c []     = c
lengthH c (_:xs) = lengthH (c+1) xs

--(lengthH 0) [1,3,6]

incBy4 :: (Integer -> Integer)
incBy4 = (+) 4  





