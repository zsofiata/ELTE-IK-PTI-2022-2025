-- catalog.inf.elte.hu

--f :: Int -> Int
-- f x = round .... 

--2^n > 10^20

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

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

isSingleton :: [a] -> Bool
isSingleton (_:[]) = True
isSingleton _      = False


isDoubleton :: [a] -> Bool
isDoubleton (x:y:[]) = True
isDoubleton _     = False

isNegative :: (Ord a,Num a) => a -> Bool
isNegative n
  | n < 0     = True
  | otherwise = False











--data Answer = Yes | Maybe | No
 
 


