module Main (main) where

import MiniLang (expression)
import Relude
import Text.Megaparsec

main :: IO ()
main = do
    parseTest (expression <* eof) "a | b || c & d && true"
