module Main (main) where

import MiniLang ()
import Relude

main :: IO ()
main = do
    args <- getArgs
    print args
