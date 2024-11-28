-- catalog.inf.elte.hu

--f :: Int -> Int
-- f x = round .... 

--2^n > 10^20

isPrime :: Integral a => a -> Bool
--isPrime n = length (divisors n) == 2
isPrime 1 = False
isPrime n = null (divisors n) 

divisors :: Integral a => a -> [a]
divisors n = [ i | i <- [2..n `div` 2], n `mod` i == 0]

primes :: Integral a => [a]
primes = [ i | i <- [2..], isPrime i]

-- divisors 12 == [1,12]
-- [1,2]
-- False

