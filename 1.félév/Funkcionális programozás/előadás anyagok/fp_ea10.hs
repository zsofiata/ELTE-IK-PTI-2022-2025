--import Prelude hiding (even,odd)
import Data.List (sort, foldl')

-- [1,2,3] ++ [4,5]
-- 1:2:3:[] ++ [4,5] -> [1,2,3,4,5]
-- 1: (2:3:[] ++ [4,5])
--    2 : (3:[] ++ [4,5])
--        3 : ([] ++ [4,5])

(+:+) :: [a] -> [a] -> [a]
(+:+) []     k  = k
(+:+) l      [] = l
(+:+) (x:xs) k  = x : (xs +:+ k)

(+::+) :: [a] -> [a] -> [a]
(+::+) = flip (foldr (:))

--flip f l k = f k l

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any' p xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' p ls = foldr (\x acc -> p x || acc) False ls

scanL :: (b -> a -> b) -> b -> [a] -> [b]
scanL f e []     = [e]
scanL f e (x:xs) = e : scanL f (f e x) xs

foldL'' f e ls = last (scanL f e ls)

scanR :: (a -> b -> b) -> b -> [a] -> [b]
scanR f e []     = e:[]
scanR f e (x:xs) = f x q : ls
  where
    ls@(q:qs) = scanR f e xs
    
-- scanr (+) 0 [1,2,3]
-- 1 + (2 + 3 + 0) -> scanr (+) 0 [2,3]
--                2 + (3 + 0) -> scanr (+) 0 [3]
--                               3 + (0) -> scanr (+) 0 []
--                                          (q:qs) = [0]       
--                               3 + 0 : [0]
--                               (q:qs) = [3,0]
--                2 + 3 : [3,0]
--                (q:qs) = [5,3,0]
-- 1 + 5 : [5,3,0]
-- [6,5,3,0]

--(f o g)(x) = f(g(x))
-- g :: a -> b
-- f :: b -> c
-- (f.g) :: a -> c

--(.) :: (b -> c) -> (a -> b) -> a -> c
--(.) f g x = f (g x)
--(.) :: (b -> c) -> (a -> b) -> (a -> c)
--(.) f g = (\x -> f (g x))
--  where
--    h x = f (g x)


--monogram "Kiefer William Frederick Dempsey George Rufus Sutherland"
-- "K. W. F. D. G. R. S."
monogram :: (String -> String)
monogram = unwords . map ((++".") . take 1) . words











    



