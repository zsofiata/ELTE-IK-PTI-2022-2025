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
(+::+) = flip . foldr $ (:)
--       f    (g     (x))
--       (f o g) (x)

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

-- take :: Int -> ([a] -> [a])
-- take 3 :: [a] -> [a]

mapMap :: (a -> b) -> [[a]] -> [[b]]
--mapMap f lls = [ [ f e | e <- ls] | ls <- lls]
--mapMap f lls = map (map f) lls
--mapMap f = map (map f)
--         f   (g   x) -> f o g (x)
mapMap = map . map
-- mapMap (+1) [[1,2], [3,4]] -> [[2,3], [4,5]]

--type String = [Char]
type Title = String
type Author = String
type Book = (Int, Author, Title, Integer, Bool)

bookTitle :: Book -> String
bookTitle (_,_,t,_,_) = t

type PredicateOn a = (a -> Bool) 
type Tuple a b = (a,b)

first :: Tuple a b -> a
first (x,y) = x

--data Bool = False | True
data Week = Mon | Tue | Wed | Thu | Fri | Sat | Sun --deriving (Show)

instance Show Week where
  show Mon = "Hetfo"
  show Tue = "Kedd"

data IntPair = P Int Int deriving (Show, Eq)

data Pair a b = P2
 { f :: a,
   s :: b} deriving (Eq, Show)

fst' :: Pair a b -> a
fst' (P2 x _) = x

--data Maybe a = Nothing | Just a

ft :: Show a => Maybe a -> String
ft (Nothing) = "semmi"
ft (Just x)  = show x

--head ((:) x xs)

-- [3, 'c']
data Union a b = I a | J b deriving (Show) 

