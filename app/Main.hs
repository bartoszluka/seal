module Main (main) where

import Relude
import Seal (eval, parseFile)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            bytes <- readFileBS filename
            let input = decodeUtf8 bytes
            case parseFile input of
                Right program -> eval program
                Left err -> putStrLn $ errorBundlePretty err
        _ -> putStrLn "1 arg needed"
