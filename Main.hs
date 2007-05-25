
module Main where

import System.Environment
import Fragment
import Check


main = do
    [x] <- getArgs
    src <- readFile x
    pre <- readFile "Include.hs"
    checkFragments pre $ parseFragments src
