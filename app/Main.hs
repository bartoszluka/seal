module Main (main) where

import Interpreter (eval)
import MiniLang (parseMiniLang)
import Relude
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            bytes <- readFileBS filename
            let input = decodeUtf8 bytes
            -- printT $ T.replace "\n" "\\n" input
            case parseMiniLang input of
                Right program -> eval program
                Left err -> putStrLn $ errorBundlePretty err
        _ -> putStrLn "1 arg needed"
