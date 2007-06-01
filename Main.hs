
module Main where

import System.Environment
import System.Directory
import Fragment
import Check


main = do
    (x:xs) <- getArgs
    src <- readFile x
    b <- doesFileExist "Include.hs"
    pre <- if b then readFile "Include.hs" else return ""
    checkFragments ("-d" `elem` xs)
                   (parseRanges $ filter (/= "-d") xs)
                   pre (parseFragments src)

parseRanges [] i = True
parseRanges xs i = f xs
    where
        f [] = False
        f (x:xs) = parseRange x i || f xs

parseRange ('.':'.':xs) i = i <= read xs
parseRange xs i = case b of
                    [] -> i == a2
                    ('.':'.':x) | null x -> i >= a2
                                | otherwise -> i >= a2 && i <= read x
    where
        (a,b) = break (== '.') xs
        a2 = read a
