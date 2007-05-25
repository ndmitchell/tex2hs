
module Check where

import Fragment
import System
import Data.List


data Result = Pass | Fail String | Missing String
              deriving (Eq,Show)


checkFragments :: String -> [Frag] -> IO ()
checkFragments prefix xs = mapM_ f xs
    where
        f (Frag i has s) | i < 262= do
            putStr $ "Checking line " ++ show i ++ "... "
            res <- check (prefix ++ "\n" ++ s)
            case res of
                Pass -> putStrLn "success"
                Fail msg -> do
                    putStrLn "FAILURE"
                    putStr $ unlines $ map ("  "++) $ lines msg
                    error "Fix your code, or we'll reject you!"

        f _ = return ()

        check s = do
            res <- checkCode s
            case res of
                Missing x -> g (Fail $ "Can't find: " ++ show x) s [t | Frag _ has t <- xs, x `elem` has]
                _ -> return res
        
        g err s [] = return err
        g err s (x:xs) = do
            r <- check (s ++ "\n" ++ x)
            if r == Pass then return Pass else g r s xs


checkCode :: String -> IO Result
checkCode s = do
    writeFile "temp.hs" s
    res <- system "ffihugs temp.hs 2> temp.txt"
    if res == ExitSuccess then return Pass else do
        x <- readFile "temp.txt"
        let s = unlines $ filter (not . null) $ drop 1 $ lines x
        return $ if any ("- Undefined" `isPrefixOf`) (tails s)
            then Missing $ takeWhile (/= '\"') $ drop 1 $ dropWhile (/= '\"') $ dropWhile (/= '-') s
            else Fail s


