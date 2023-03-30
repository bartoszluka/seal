module Main (main) where

import Data.Text.IO qualified as T
import Relude
import Seal (eval, parseFile)
import Seal.Compiler (codegen, typecheck)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [command, filename] ->
            case command of
                "compile" -> compile
                "c" -> compile
                "interpret" -> interpret
                "i" -> interpret
                _ -> undefined
          where
            -- readAndParse :: IO Program
            -- readAndParse = do
            --     bytes <- readFileBS filename
            --     let input = decodeUtf8 bytes
            --     case parseFile input of
            --         Right program -> return program
            --         Left err -> putStrLn $ errorBundlePretty err

            compile :: IO ()
            compile = do
                bytes <- readFileBS filename
                let input = decodeUtf8 bytes
                case parseFile input of
                    Right program -> do
                        case typecheck program of
                            Right compiled -> T.putStrLn $ codegen program
                            Left errors -> print errors
                    Left err -> putStrLn $ errorBundlePretty err

            interpret = do
                bytes <- readFileBS filename
                let input = decodeUtf8 bytes
                case parseFile input of
                    Right program -> do
                        maybeErrors <- eval program
                        case maybeErrors of
                            Right _ -> return ()
                            Left errors -> print errors
                    Left err -> putStrLn $ errorBundlePretty err
        _ -> putStrLn "1 arg needed"
