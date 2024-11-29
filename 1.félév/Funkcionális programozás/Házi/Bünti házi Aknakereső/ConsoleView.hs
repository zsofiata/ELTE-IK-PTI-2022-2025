{-# LANGUAGE ViewPatterns #-}

module MineSweeper.ConsoleView where

import System.Random ( randomIO )
import System.IO ( stdout, hFlush )
import MineSweeper.Model
    ( startNewGame
    , stepGame
    , Board(Board)
    , Field(Flagged, Closed, Opened)
    , FieldType(..)
    , Game(Game, gameState)
    , MoveType(..)
    , State(..) )
import Text.Read ( readMaybe )
import Data.List ( intercalate, genericReplicate, intersperse, transpose )

instance Show FieldType where
    show Mine = "X"
    show (Free x) = show x

instance Show Field where
    show (Closed _) = "_"
    show (Opened x) = show x
    show (Flagged _) = "F"

instance Show Board where
    show (Board w h x) = intercalate "\n" ((map ((genericReplicate n ' ' ++) . intersperse ' ') $ transpose $ map (format (numberOfDigits w) . show) [0..w-1]) ++ zipWith (\row num -> format n (show num) ++ intercalate "|" (map show row)) x [0..])
        where
            n = numberOfDigits h + 1

format :: Integral a => a -> String -> String
format n [] = genericReplicate n ' '
format n (x:xs) = x : format (n-1) xs

numberOfDigits :: (Integral a, Num b) => a -> b
numberOfDigits x = 1 + if (-9) <= x && x <= 9 then 0 else numberOfDigits (x `quot` 10)

instance Show State where
    show Running = ""
    show (GameOver p)
        | p           = "Nyertél!"
        | otherwise   = "Vesztettél! Bombát nyitottál!"

instance Show Game where
    show (Game b s) = intercalate "\n" [show b, show s]

iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

prompt :: Read a => String -> IO a
prompt str = putStr' str >> getReadable

getReadable :: Read a => IO a
getReadable = do
        input <- getLine
        case readMaybe input of
            Nothing -> putStrLn "Nem megfelelő típus! Próbálja újra!" >> getReadable
            Just x -> return x

readMaybeMoveType :: String -> Maybe MoveType
readMaybeMoveType str = case words str of
    "F":(map readMaybe -> [Just a, Just b]) -> Just $ Flag (a, b)
    (map readMaybe -> [Just a, Just b]) -> Just $ Open (a, b)
    _ -> Nothing

main :: IO ()
main = do
    seed <- randomIO :: IO Int
    w <- prompt "Adja meg a tábla szélességét: " :: IO Int
    h <- prompt "Adja meg a tábla magasságát: " :: IO Int
    b <- prompt "Adja meg a bombák számát!\n(Tábla méretének 10%-a és 50%-a között lesz a bombák száma): " :: IO Int
    let initGame = startNewGame w h b seed
    print initGame
    iterateUntilM (\game -> case gameState game of Running -> False; _ -> True) (\game -> do
        move <- getLine
        case readMaybeMoveType move of
            Just x -> do 
                let next = stepGame game x
                print next
                return next
            _ -> do
                putStrLn "Nem megfelelő beviteli formátum!"
                print game
                return game
        ) initGame
    return ()