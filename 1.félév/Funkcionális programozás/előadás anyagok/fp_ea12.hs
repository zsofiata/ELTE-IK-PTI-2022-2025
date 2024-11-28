
data D = D Int deriving (Show)

newtype N = N Int deriving (Show)

testD :: D -> Int
testD (D _) = 5

testN :: N -> Int
testN (N _) = 5

g :: Num a => a -> Int -> [a]
g = undefined

data Strict a = S !a deriving (Show)

data Lazy a = L a deriving (Show)

testS :: Strict a -> Int
testS (S _) = 5

testL :: Lazy a -> Int
testL (L _) = 5

data Peano = Zero | Succ Peano deriving (Eq, Show)

fromInt :: Int -> Peano
fromInt 0 = Zero
fromInt n = Succ (fromInt (n-1))

addInt :: Int -> Int -> Int
addInt 0 m = m
addInt n m = 1 + (addInt (n-1) m)

addPeano :: Peano -> Peano -> Peano
addPeano Zero     m = m
addPeano (Succ n) m = Succ (addPeano n m)

fromPeano :: Peano -> Int
fromPeano Zero = 0
fromPeano (Succ n) = 1 + fromPeano n 

-- foldLP :: 

infixr 5 `Cons`
data List a = Nil | Cons a (List a) deriving (Show)
--   [Int]     []     Int : [Int]

fromL :: [a] -> List a
fromL []     = Nil
fromL (x:xs) = x `Cons` fromL xs



lengthL :: List a -> Int
lengthL Nil = 0
lengthL (Cons _ xs) = 1 + lengthL xs


sumL :: Num a => List a -> a
sumL Nil = 0
sumL (Cons x xs) = x + sumL xs
 

