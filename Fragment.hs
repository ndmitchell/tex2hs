
module Fragment where

import Data.List
import Data.Char
import Data.Maybe


type Line = Int

data Frag = Frag Line [String] String
            deriving Show



parseFragments :: String -> [Frag]
parseFragments x = f $ zip [1..] (lines x)
    where
        f ((i,x):xs) | beginCode x = let (a,b) = break (endCode . snd) xs
                                         code = (unlines $ map snd a)
                                     in Frag i (provides code) (tweak code) : f b
                     | ignoreCode x = f $ drop 1 $ dropWhile (not . endCode . snd) xs
                     | otherwise = concat (zipWith (g i) [1..] (parseLine x)) ++ f xs
        f [] = []

        g line col s | length ws <= 1 = []
                     | "=" `elem` ws = [Frag line [head ws] s]
                     | ws !! 1 == "::" = [Frag line [head ws] (s ++ "\n" ++ head ws ++ " = undefined")]
                     | otherwise = [Frag line [] (name ++ " _ = " ++ s)]
            where
                ws = words s
                name = "expr_" ++ show line ++ "_" ++ show col

ignoreCode = isPrefixOf "\\ignore\\begin{code}"
beginCode = isPrefixOf "\\begin{code}"
endCode = isPrefixOf "\\end{code}"


parseLine :: String -> [String]
parseLine ('|':'|':xs) = parseLine xs
parseLine ('|':xs) = a : parseLine b
    where (a,b) = parseBar xs
parseLine xs | "\\ignore|" `isPrefixOf` xs = parseLine $ snd $ parseBar $ drop 8 xs
parseLine (x:xs) = parseLine xs
parseLine [] = []

parseBar ('|':'|':xs) = ('|':a,b)
    where (a,b) = parseBar xs
parseBar ('|':xs) = ("",xs)
parseBar (x:xs) = (x:a,b)
    where (a,b) = parseBar xs
parseBar [] = ("","")


provides :: String -> [String]
provides = f . lines
    where
        f (x:(y:ys):xs) | isSpace y = f ((x ++ y:ys):xs)
        f (x:xs) | "data " `isPrefixOf` x || "type " `isPrefixOf` x = providesData x ++ f xs
        f (x:xs) = [head $ words x | not $ null x] ++ f xs
        f _ = []


providesData :: String -> [String]
providesData x = (ws!!1) : [a2 | a1:a2:as <- tails ws, a1 `elem` ["|","="]]
    where ws = words x


tweak :: String -> String
tweak = unlines . f . lines
    where
        f (x:xs) | isJust typ && typ /= fun = x : def : f xs
            where
                def = fromJust typ ++ " = undefined"
                typ = isType x
                fun = isFunc xs
        
        f (x:xs) = x : f xs
        f [] = []


isType :: String -> Maybe String
isType x = if length xs > 1 && (xs !! 1) == "::" then Just (head xs) else Nothing
    where xs = words x

isFunc :: [String] -> Maybe String
isFunc (x:xs) = if length xs > 0 then Just (head xs) else Nothing
    where xs = words x
isFunc _ = Nothing
