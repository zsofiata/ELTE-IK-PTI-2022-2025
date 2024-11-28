--import Prelude hiding (even,odd)
import Data.List (sort)
 

--rev [1,2,3] []
--rev [2,3]   [1]
--rev [3]     [2,1]
--rev []      [3,2,1]
--[3,2,1]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' ls = foldR (\x acc -> acc ++ [x]) [] ls

-- take :: Int -> ([a] -> [a])
-- f :: a -> (a  -> (a -> a))

take10 :: [a] -> [a]
take10 = take 10

-- take10 [1..20]
-- take 10 [1..20]

map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

sqrEvery :: Num a => [a] -> [a]
sqrEvery = map (^2)
--sqrEvery [] = []
--sqrEvery (x:xs) = x ^ 2 : sqrEvery xs

g :: Num a => [a] -> [a]
g = map (^3)
--g [] = []
--g (x:xs) = x ^ 3 : g xs

-- [1,2,3,0] "abcde" -> "a" : "ab" : "abc" : "" : []
--prefixes :: [Int] -> [a] -> [[a]]
--prefixes [] _ = []
--prefixes (i:is) ls = take i ls : prefixes is ls
--prefixes is ls = map (take' ls) is
--  where
--    take' ls i = take i ls
--prefixes is ls = map (`take` ls) is
--prefixes is ls = map (\i -> take i ls) is
prefixes is ls = map (flip take ls) is
-- (\even -> even 1) -- (+1)

ff even = Data.List.sort even
-- ff odd

flip' :: (a -> b -> c) -> b  -> a -> c
flip' f b a = f a b

-- flip' take [2,4..] 10
--  take 10 [2,4..]

-- f $ e = f e
-- ($) f e = f e

sum' :: Num a => [a] -> a
--sum' []     = 0
--sum' (x:xs) = x + sum xs
sum' = foldR (+) 0
-- (sum') [1..10] -> (fold (+) 0) [1..10]
 
product' :: Num a => [a] -> a
--product' [] = 1
--product' (x:xs) = x * product' xs
product' = foldR (*) 1

foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f e [] = e
foldR f e (x:xs) = x `f` foldR f e xs

length' :: [a] -> Int
--length' [] = 0
--length' (_:xs) = 1 + length' xs

length' = foldR (\_ acc -> 1 + acc) 0

--maP :: (a -> b) -> [a] -> [b]
--maP f ls = foldR 



