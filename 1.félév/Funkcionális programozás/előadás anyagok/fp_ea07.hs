

head' :: [a] -> a
head' (x:xs) = x
--head' []     = error "Üres lista"
--isSingleton (x:(y:[]))

tail' :: [a] -> [a]
tail' (x:xs) = xs
tail' []     = error "asda"

last' :: [a] -> a
last' (x:[]) = x
last' (_:xs) = last' xs

init' :: [a] -> [a]
init' (x:[]) = []
init' (x:xs) = x : init' xs

--take n l | n <= 0 = []
f (x:cons@(y:ys)) = cons

--rev [1,2,3] []
--rev [2,3]   [1]
--rev [3]     [2,1]
--rev []      [3,2,1]
--[3,2,1]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

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

