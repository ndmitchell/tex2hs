
module Fragment where

import Data.List
import Data.Char
import Data.Maybe


type Line = Int

data Frag = Stmt Line [String] String
          | Expr Line String
            deriving Show



parseFragments :: String -> [Frag]
parseFragments x = f $ zip [1..] (lines x)
    where
        f ((i,x):xs) | beginCode x = let (a,b) = break (endCode . snd) xs
                                         code = (unlines $ map snd a)
                                     in Stmt i (provides code) (tweak code) : f b
                     | ignoreCode x = f $ drop 1 $ dropWhile (not . endCode . snd) xs
                     | otherwise = concat (zipWith (g i) [1..] (parseLine x)) ++ f xs
        f [] = []

        g line col s | null ws = []
                     | "=" `elem` ws = [Stmt line [head ws] s]
                     | length ws > 1 && ws !! 1 == "::" = [Stmt line [head ws] (s ++ "\n" ++ head ws ++ " = undefined")]
                     | otherwise = [Expr line s]
            where
                ws = words s

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
        f (x:(y:ys):xs) | not ("class " `isPrefixOf` x) && not (" " `isPrefixOf` x) && isSpace y = f ((x ++ y:ys):xs)
        f (x:xs) | "data " `isPrefixOf` x || "type " `isPrefixOf` x = providesData x ++ f xs
        f (x:xs) | "class " `isPrefixOf` x = providesClass x ++ f xs
        f (x:xs) = [unbracket $ head $ words x | not $ null x] ++ f xs
        f _ = []

unbracket ('(':xs) = init xs
unbracket x = x

providesClass :: String -> [String]
providesClass x = [pre !! 1]
    where
        pre = if "=>" `elem` ws then dropWhile (/= "=>") ws else ws
        ws = words x


providesData :: String -> [String]
providesData x = (ws!!1) : concat [f (a2:as) | a1:a2:as <- tails ws, a1 `elem` ["|","="]]
    where
        ws = words x
        f ((x:xs):_) | isUpper x = [x:xs]
        f xs = take 1 $ filter (":" `isPrefixOf`) xs


tweak :: String -> String
tweak = unlines . f . lines
    where
        f (x:xs)
            | "module " `isPrefixOf` x = f xs
            | "import " `isPrefixOf` x = f xs

        f (x:xs) | isJust typ && not (isFunc (fromJust typ) xs) = x : def : f xs
            where
                def = "(" ++ fromJust typ ++ ") = undefined"
                typ = isType x
        
        f (x:xs) = x : f xs
        f [] = []


isType :: String -> Maybe String
isType x | all isSpace (take 1 x) = Nothing
         | otherwise = case lexemes x of
                            (a:"::":_) -> Just a
                            ("(":a:")":"::":_) -> Just a
                            _ -> Nothing


isFunc :: String -> [String] -> Bool
isFunc name xs = name `elem` ws
    where ws = takeWhile (/= "=") $ lexemes $ concat $ take 1 xs


lexemes :: String -> [String]
lexemes x = case lex x of
    [("",_)] -> []
    [(x,y)] -> x : lexemes y
