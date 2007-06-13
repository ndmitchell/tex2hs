
module Fragment where

import Haskell.Provides
import Haskell.Tweak
import Latex.Parser


data Frag = Stmt Line [String] String
          | Expr Line String
            deriving Show


parseFragments :: String -> [Frag]
parseFragments = map f . parseLatex
    where
        pre = if "=>" `elem` ws then dropWhile (/= "=>") ws else ws
        ws = words x


providesData :: String -> [String]
providesData x = (ws!!1) : concat [f $ takeWhile (/= "|") (a2:as) | a1:a2:as <- tails ws, a1 `elem` ["|","="]]
    where
        ws = words x
        f xs | not $ null res = res
            where res = take 1 $ filter (":" `isPrefixOf`) xs
        f ((x:xs):_) | isUpper x = [x:xs]
        f _ = []


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


