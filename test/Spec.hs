{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

import Relude

import Seal.Compiler (Error (..), codegen, typecheck)
import Seal.Interpreter
import Seal.Parser (
    Expression (..),
    FunctionDefinition (..),
    LetBinding (LetBinding),
    Parser,
    ParserError,
    Statement (..),
    VarType (..),
    blockExpression,
    boolLiteral,
    declaration,
    doubleLiteral,
    expression,
    identifierName,
    intLiteral,
    parseFile,
 )

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Data.Text.IO qualified as T
import Relude.Unsafe qualified as Unsafe
import System.Directory (listDirectory, withCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (addExtension, isExtensionOf, replaceExtension, takeBaseName, (-<.>), (</>))
import System.IO (hClose, hGetContents)
import System.Process
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Hspec.Core.Spec (fromSpecList, specItem)
import Test.Hspec.Golden
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Test.Hspec.QuickCheck (prop)
import Test.Tasty.Silver
import Text.Megaparsec (eof, parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf
import Text.RawString.QQ

only :: Parser a -> Text -> Either ParserError a
only parser = parse (parser <* eof) ""

main :: IO ()
main = hspec $ do
    describe "parsing" $ do
        describe "program" $ do
            it "parses program with no statements and some whitespace" $
                parseFile
                    [r||]
                    `shouldParse` []

            it "parses program with empty main" $
                parseFile
                    [r|fn main() = 69|]
                    `shouldParse` [ FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "main"
                                        , functionBody = IntLiteral 69
                                        , functionArgs = []
                                        }
                                  ]

            it "parses program with main with a block expression" $
                parseFile
                    [r|fn main() = {
                        let i = 0
                        let b = i
                        420
                    }
                    |]
                    `shouldParse` [ FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "main"
                                        , functionBody =
                                            Block
                                                [LetBinding "i" (IntLiteral 0), LetBinding "b" (Identifier "i")]
                                                (IntLiteral 420)
                                        , functionArgs = []
                                        }
                                  ]

            it "parses program with more functions" $
                parseFile
                    [r|
                    fn dupa () = 69

                    fn anotherOne (arg1, arg2) = 1

                    fn main() = {
                        let i = 0
                        let b = i
                        420
                    }
                    |]
                    `shouldParse` [ FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "dupa"
                                        , functionBody = IntLiteral 69
                                        , functionArgs = []
                                        }
                                  , FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "anotherOne"
                                        , functionBody = IntLiteral 1
                                        , functionArgs = [("arg1", Nothing), ("arg2", Nothing)]
                                        }
                                  , FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "main"
                                        , functionBody =
                                            Block
                                                [LetBinding "i" (IntLiteral 0), LetBinding "b" (Identifier "i")]
                                                (IntLiteral 420)
                                        , functionArgs = []
                                        }
                                  ]
            it "parses program with more functions with arguments with types" $
                parseFile
                    [r|
                    fn anotherOne (arg1: Dupa, arg2: Another) = 1

                    fn main() = {
                        let i = 0
                        let b = i
                        420
                    }
                    |]
                    `shouldParse` [ FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "anotherOne"
                                        , functionBody = IntLiteral 1
                                        , functionArgs = [("arg1", Just "Dupa"), ("arg2", Just "Another")]
                                        }
                                  , FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "main"
                                        , functionBody =
                                            Block
                                                [LetBinding "i" (IntLiteral 0), LetBinding "b" (Identifier "i")]
                                                (IntLiteral 420)
                                        , functionArgs = []
                                        }
                                  ]
            it "parses program with declarations and comments" $
                parseFile
                    [r|// program start
                        fn main() = 69
                        //program end
                    |]
                    `shouldParse` [ FunctionDefinition
                                        { functionReturnType = Nothing
                                        , functionName = "main"
                                        , functionBody = IntLiteral 69
                                        , functionArgs = []
                                        }
                                  ]
        -- TODO: uncomment when implemented
        -- it "parses a program with an if statement" $
        --     parseFile
        --         [r| program
        --             {
        --                 if(harryPotter)
        --                      read anotherBook;
        --                 then
        --             }
        --         |]
        --         `shouldParse` ([], [StIf (Identifier "harryPotter") (StRead "anotherBook")])
        -- it "parses a program with an if-else statement" $
        --     parseFile
        --         [r| program
        --             {
        --                  if(harryPotter)
        --                      read anotherBook;
        --                  else
        --                      allGood;
        --             }
        --         |]
        --         `shouldParse` ( []
        --                       ,
        --                           [ StIfElse
        --                                 (Identifier "harryPotter")
        --                                 (StRead "anotherBook")
        --                                 (StExpression (Identifier "allGood"))
        --                           ]
        --                       )
        -- it "parses a program with a nested if-else statement" $
        --     parseFile
        --         [r| program
        --             {
        --                  if(harryPotter)
        --                      if(count>2)
        --                          read anotherBook;
        --                      else
        --                          allGood;
        --             }
        --         |]
        --         `shouldParse` ( []
        --                       ,
        --                           [ StIf
        --                                 (Identifier "harryPotter")
        --                                 ( StIfElse
        --                                     (GreaterThen (Identifier "count") (IntLiteral 2))
        --                                     (StRead "anotherBook")
        --                                     (StExpression (Identifier "allGood"))
        --                                 )
        --                           ]
        --                       )
        -- it "parses a program with a while statement" $
        --     parseFile
        --         [r| program
        --             {
        --                  while(alive)
        --                      write code;
        --             }
        --         |]
        --         `shouldParse` ([], [StWhile (Identifier "alive") (StWriteExpr (Identifier "code"))])
        describe "identifiers" $ do
            it "parses identifiers with only letters" $ only identifierName "abcd" `shouldParse` "abcd"
            it "parses identifiers with letters and numbers" $ only identifierName "ABCeJP2gmD" `shouldParse` "ABCeJP2gmD"
            it "does not parse identifiers with '_' (underscore)" $ only identifierName `shouldFailOn` "_AbCd_234"
            it "does not parse identifiers that start with numbers" $ only identifierName `shouldFailOn` "2344AbCd234"

        describe "expressions" $ do
            describe "int literals" $ do
                it "parses 1 as int" $
                    only intLiteral "1" `shouldParse` IntLiteral 1
                it "parses 69 as int" $
                    only intLiteral "69" `shouldParse` IntLiteral 69
                it "parses 10000 as int" $
                    only intLiteral "10000" `shouldParse` IntLiteral 10000
                it "parses 0 as int" $
                    only intLiteral "0" `shouldParse` IntLiteral 0
                prop "parses int literals" $
                    \a ->
                        let n = abs a
                         in only intLiteral (show n) `shouldParse` IntLiteral n

            describe "bool literals" $ do
                it "parses 'false' as bool" $
                    only boolLiteral "false" `shouldParse` BoolLiteral False
                it "parses 'true' as bool" $
                    only boolLiteral "true" `shouldParse` BoolLiteral True

            describe "double literals" $ do
                it "parses 1 as double" $
                    only doubleLiteral "1.0" `shouldParse` DoubleLiteral 1
                it "parses 69 as double" $
                    only doubleLiteral "69.0" `shouldParse` DoubleLiteral 69
                it "parses 10000 as double" $
                    only doubleLiteral "10000.0" `shouldParse` DoubleLiteral 10000
                it "parses 0 as double" $
                    only doubleLiteral "0.0" `shouldParse` DoubleLiteral 0
                prop "parses double literals" $
                    \x ->
                        let positive = abs x
                         in only doubleLiteral (T.pack $ printf "%f" positive)
                                `shouldParse` DoubleLiteral positive
            describe "operators" $ do
                describe "unary" $ do
                    it "parses an int cast" $
                        only expression "(int) a"
                            `shouldParse` IntCast (Identifier "a")

                    it "parses a combination of unary operators with right associativity" $
                        only expression "- ~ !! ( int  ) -  (   double ) a  "
                            `shouldParse` UnaryMinus (BitwiseNeg (LogicalNeg (LogicalNeg (IntCast (UnaryMinus (DoubleCast (Identifier "a")))))))

                    it "does not parse if there is no expression on the right" $
                        only expression `shouldFailOn` "!(int)"
                describe "bitwise binary" $ do
                    it "parses a bitwise multiplication operator" $
                        only expression "a & b"
                            `shouldParse` BitwiseMult (Identifier "a") (Identifier "b")

                    it "parses a bitwise sum operator" $
                        only expression "a | b"
                            `shouldParse` BitwiseSum (Identifier "a") (Identifier "b")

                    it "operations are left-associative" $
                        only expression "1 | 2 & 3"
                            `shouldParse` BitwiseMult
                                (BitwiseSum (IntLiteral 1) (IntLiteral 2))
                                (IntLiteral 3)

                    it "parses bitwise and unary operators in the same expression" $
                        only expression "1 | - 3 & ~false"
                            `shouldParse` BitwiseMult
                                (BitwiseSum (IntLiteral 1) (UnaryMinus (IntLiteral 3)))
                                (BitwiseNeg (BoolLiteral False))
                    it "allows overriding precedence with parens" $
                        only expression "!(1 | 2)"
                            `shouldParse` LogicalNeg (BitwiseSum (IntLiteral 1) (IntLiteral 2))

                it "parses adding 2 numbers" $
                    only expression "0   + 6.9 " `shouldParse` Addition (IntLiteral 0) (DoubleLiteral 6.9)
                it "parses comparing 2 numbers" $
                    only expression "1>0" `shouldParse` GreaterThen (IntLiteral 1) (IntLiteral 0)
                it "parses addition and multiplication precedence" $
                    only expression "a   +2*3"
                        `shouldParse` Addition
                            (Identifier "a")
                            ( Multiplication
                                (IntLiteral 2)
                                (IntLiteral 3)
                            )
                it "parses overriding precedence with parens" $
                    only expression "(a+2)*3"
                        `shouldParse` Multiplication
                            ( Addition
                                (Identifier "a")
                                (IntLiteral 2)
                            )
                            (IntLiteral 3)
                it "parses logic operators" $
                    only expression "a || true && false   "
                        `shouldParse` LogicAnd
                            ( LogicOr
                                (Identifier "a")
                                (BoolLiteral True)
                            )
                            (BoolLiteral False)
                it "distinguishes logic and bitwise operators" $
                    only expression "a | b || c & d && true"
                        `shouldParse` LogicAnd
                            ( LogicOr
                                (BitwiseSum (Identifier "a") (Identifier "b"))
                                (BitwiseMult (Identifier "c") (Identifier "d"))
                            )
                            (BoolLiteral True)
                it "parses all operators with correct precedence" $
                    only expression "a || b != c + d / e & f"
                        `shouldParse` LogicOr
                            (Identifier "a")
                            ( NotEqual
                                (Identifier "b")
                                ( Addition
                                    (Identifier "c")
                                    ( Division
                                        (Identifier "d")
                                        ( BitwiseMult
                                            (Identifier "e")
                                            (Identifier "f")
                                        )
                                    )
                                )
                            )

        describe "blocks" $ do
            it "parses block expression with no let bindings" $ only blockExpression "{ 0 }" `shouldParse` Block [] (IntLiteral 0)
            it "parses block expression with a let binding on the same line" $
                only blockExpression "{ let a = 0 0 }" `shouldParse` Block [LetBinding "a" (IntLiteral 0)] (IntLiteral 0)

            it "parses block expression with a let binding on different lines" $
                only
                    blockExpression
                    [r|{ let a = 0
                    0 }|]
                    `shouldParse` Block [LetBinding "a" (IntLiteral 0)] (IntLiteral 0)

            it "parses block expression with many let bindings" $
                only
                    blockExpression
                    [r|{ let a = 0
                        let jp = 2
                    0 }|]
                    `shouldParse` Block [LetBinding "a" (IntLiteral 0), LetBinding "jp" (IntLiteral 2)] (IntLiteral 0)
    describe "evaluate" $ do
        let emptyScope = evalExpression HM.empty
        prop "evaluates int literals" $
            \n ->
                emptyScope (IntLiteral n) `shouldBe` Right (VInt n)
        prop "evaluates int literals with unary minus" $
            \a ->
                let n = abs a
                 in emptyScope (UnaryMinus (IntLiteral n)) `shouldBe` Right (VInt (-n))
        it "evaluates sum of ints" $
            emptyScope (IntLiteral 1 `Addition` IntLiteral 2) `shouldBe` Right (VInt 3)
        it "evaluates subtraction sum of ints" $
            emptyScope (IntLiteral 8 `Subtraction` IntLiteral 2) `shouldBe` Right (VInt 6)
        it "evaluates multiplication of ints" $
            emptyScope (IntLiteral 21 `Multiplication` IntLiteral 37) `shouldBe` Right (VInt 777)
        it "evaluates divisio of ints" $
            emptyScope (IntLiteral 8 `Division` IntLiteral 3) `shouldBe` Right (VInt 2)
        it "evaluates bitwise sum of ints" $
            emptyScope (IntLiteral 8 `BitwiseSum` IntLiteral 2) `shouldBe` Right (VInt 10)
        prop "any number bitwise or'ed with -1 is -1" $
            \n -> emptyScope (IntLiteral (-1) `BitwiseSum` IntLiteral n) `shouldBe` Right (VInt (-1))
        prop "any number bitwise and'ed with -1 is this number" $
            \n -> emptyScope (IntLiteral (-1) `BitwiseMult` IntLiteral n) `shouldBe` Right (VInt n)
        it "bitwise negation of -1 is 0" $
            emptyScope (BitwiseNeg (IntLiteral (-1))) `shouldBe` Right (VInt 0)
        describe "comparisons" $ do
            it "evaluates greater than on ints" $
                emptyScope (IntLiteral 21 `GreaterThen` IntLiteral 37) `shouldBe` Right (VBool False)
            it "evaluates greater than on an int and a double" $
                emptyScope (IntLiteral 21 `GreaterThen` DoubleLiteral 37) `shouldBe` Right (VBool False)
            it "evaluates greater than or equal on an int and a double" $
                emptyScope (IntLiteral 21 `GreaterThenEq` DoubleLiteral 37) `shouldBe` Right (VBool False)
            prop "less than and greater or equal are opposite (on an int and a double)" $ \(n, m) ->
                emptyScope (IntLiteral n `GreaterThenEq` DoubleLiteral m)
                    `shouldBe` emptyScope (LogicalNeg (IntLiteral n `LessThen` DoubleLiteral m))
            it "true is equal to true" $
                emptyScope (BoolLiteral True `Equal` BoolLiteral True) `shouldBe` Right (VBool True)
            it "false is not equal to true" $
                emptyScope (BoolLiteral False `Equal` BoolLiteral True) `shouldBe` Right (VBool False)
    -- TODO: uncomment when implemented
    -- describe "typecheck" $ do
    --     it "accepts a program with boolean expression as a condition" $
    --         typecheck ([], [StIf (BoolLiteral True) StReturn]) `shouldBe` Right ()
    --     it "does not accept a program with int expression as a condition" $
    --         typecheck ([], [StIf (IntLiteral 69) StReturn]) `shouldBe` Left (EType TypeBool (VInt 69) :| [])
    --     xit "does not accept a program with int expression as a condition" $
    --         typecheck ([("a", TypeInt)], [StAssignment "a" (IntLiteral 69), StIf (Identifier "a") StReturn]) `shouldBe` Left (EType TypeBool (VInt 69) :| [])

    xdescribe "compiler" $ do
        sealFiles <- runIO $ findByExtension [".seal"] "test/programs/"
        forM_ sealFiles $ \sealFile -> do
            it (takeBaseName sealFile) $ runGoldenTestForFile sealFile

runGoldenTestForFile :: FilePath -> IO (Golden Text)
runGoldenTestForFile sealFile = do
    input <- T.readFile sealFile
    llvmCode <- failOnLeft $ generateCode input
    let llFile = addExtension sealFile ".ll"
    writeFileText llFile llvmCode
    result <- testWithLli llFile
    output <- failOnLeft result
    return $ goldenTest sealFile (T.pack output)
  where
    generateCode :: Text -> Either String Text
    generateCode input = case parseFile input of
        Right program -> do
            case typecheck program of
                Right () -> Right $ codegen program
                Left errors -> Left $ show errors
        Left errors -> Left $ errorBundlePretty errors
    testWithLli llFile = do
        (_, Just outHandle, Just errorHandle, processHandle) <-
            createProcess
                (proc "lli" [llFile])
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        output <- hGetContents outHandle
        errors <- hGetContents errorHandle
        exitCode <- waitForProcess processHandle
        return $ case exitCode of
            ExitSuccess -> Right output
            ExitFailure _ -> Left errors

    goldenTest :: FilePath -> Text -> Golden Text
    goldenTest name actualOutput =
        Golden
            { output = actualOutput
            , encodePretty = T.unpack
            , writeToFile = T.writeFile
            , readFromFile = T.readFile
            , goldenFile = addExtension name ".gold"
            , actualFile = Nothing
            , failFirstTime = True
            }

    failOnLeft :: (Show a) => Either String a -> IO a
    failOnLeft result = do
        result `shouldSatisfy` isRight
        return $ Unsafe.fromJust $ rightToMaybe result
