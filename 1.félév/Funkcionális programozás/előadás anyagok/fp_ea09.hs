--import Prelude hiding (even,odd)
import Data.List (sort, foldl')

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

maP :: (a -> b) -> [a] -> [b]
--maP f ls = foldR (\x acc -> f x : acc) [] ls
maP f ls = foldr step [] ls
  where
    --i = length ls
    step x acc = f x : acc -- i

filteR :: (a -> Bool) -> [a] -> [a] 
--filteR p [] = []
--filteR p (x:xs)
--  | p x       = x : filteR p xs
--  | otherwise = filteR p xs

filteR p ls = foldr step [] ls
  where
    step x acc
      | p x       = x : acc
      | otherwise = acc

-- filteR even [1,2,3,4]
-- foldr step [] [1,2,3,4]
-- step 1 (foldr step [] [2,3,4])
--        (foldr step [] [2,3,4])
--        step 2 (foldr step [] [3,4])
--        2 : (foldr step [] [3,4])
--            step 3 (foldr step [] [4])
--                   (foldr step [] [4])
--                   step 4 (foldr step [] [])
--                   4 :    []

lengtH :: [a] -> Int
lengtH ls = len 0 ls
  where
  len i [] = i
  len i (_:xs) = len (i+1) xs 

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL f e [] = e
foldL f e (x:xs) = foldL f (e `f` x) xs


lengthH' :: [a] -> Int
lengthH' ls = foldl (\ acc _ -> acc + 1) 0 ls

lengthH'' ls = foldl' (\ acc _ -> acc + 1 ) 0 ls

-- foldL (+) 0 [1,2,3]
-- foldL (+) 1 [2,3]
-- foldL (+) 3 [3]
-- foldL (+) 6 []
-- 6

and' :: [Bool] -> Bool
--and' [] = True
--and' (x:xs) = x && and' xs
and' ls = foldr (&&) True ls


and'' :: [Bool] -> Bool
{-
and'' ls = andH True ls
  where
    andH b []     = b
    andH b (x:xs) = andH (b && x) xs
-}
and'' ls = foldl (&&) True ls
and''' ls = foldl' (&&) True ls


-- [1,2,3] ++ [4,5]
-- 1:2:3:[] ++ [4,5] -> [1,2,3,4,5]
-- 1: (2:3:[] ++ [4,5])
--    2 : (3:[] ++ [4,5])
--        3 : ([] ++ [4,5])

(+:+) :: [a] -> [a] -> [a]
(+:+) []     k  = k
(+:+) l      [] = l
(+:+) (x:xs) k  = x : (xs +:+ k)

