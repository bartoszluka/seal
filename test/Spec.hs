{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Relude
import Test.Hspec
import Text.Megaparsec (eof, errorBundlePretty, parse, parseMaybe)
import Text.RawString.QQ

import Data.Text qualified as T
import MiniLang (
    Declaration (..),
    Expression (..),
    Parser,
    boolLiteral,
    declaration,
    doubleLiteral,
    identifierName,
    intLiteral,
    pExpr,
    parseMiniLang,
 )
import Test.Hspec.QuickCheck (prop)
import Text.Printf

parseEither :: Parser a -> Text -> Either Text a
parseEither parser input = mapError errorBundlePretty parsed
  where
    mapError function err = case err of
        Left l -> Left $ T.pack $ function l
        Right ok -> Right ok
    parsed = parse (parser <* eof) "" input

main :: IO ()
main = hspec $ do
    describe "parsing" $ do
        describe "program" $ do
            it "parses program with no statements" $
                parseMiniLang "program{}" `shouldSatisfy` isRight
            it "parses program with no statements and some whitespace" $ do
                parseMiniLang
                    [r|program {

                    }
                    |]
                    `shouldSatisfy` isRight

            it "parses program with declarations" $ do
                parseMiniLang
                    [r|program {
                        int i; double d;
                        bool c;
                        bool doopy;
                    }
                    |]
                    `shouldBe` Right [TypeInt "i", TypeDouble "d", TypeBool "c", TypeBool "doopy"]

            it "parses program with declarations and comments" $ do
                parseMiniLang
                    [r|// program start
                        program 
                        {
                            int i;double d;
                            bool c;
                            bool doopy;
                        }
                        //program end
                    |]
                    `shouldBe` Right [TypeInt "i", TypeDouble "d", TypeBool "c", TypeBool "doopy"]
        describe "identifiers" $ do
            it "parses identifiers with only letters" $ do
                parseEither identifierName "abcd" `shouldBe` Right "abcd"
            it "parses identifiers with letters and numbers" $ do
                parseEither identifierName "ABCeJP2gmD" `shouldBe` Right "ABCeJP2gmD"
            it "does not parse identifiers with '_' (underscore)" $ do
                parseEither identifierName "_AbCd_234" `shouldNotBe` Right "_AbCd_234"
            it "does not parse identifiers that start with numbers" $ do
                parseEither identifierName "2344AbCd234" `shouldSatisfy` isLeft

        describe "declarations" $ do
            it "parses declaration of an int variable" $
                parseMaybe declaration "int i;" `shouldBe` Just (TypeInt "i")
            it "parses declaration of an int variable with different ammount of whitespace" $ do
                parseEither
                    declaration
                    [r|int     
                                    abcd123
                                    
                                    ;|]
                    `shouldBe` Right (TypeInt "abcd123")
            it "parses declaration of a 'double' variable" $
                parseMaybe declaration "double i;" `shouldBe` Just (TypeDouble "i")
            it "parses declaration of a 'bool' variable" $
                parseMaybe declaration "bool i;" `shouldBe` Just (TypeBool "i")

        describe "expressions" $ do
            describe "int literals" $ do
                it "parses 1 as int" $
                    parseEither intLiteral "1" `shouldBe` Right (IntLiteral 1)
                it "parses 69 as int" $
                    parseEither intLiteral "69" `shouldBe` Right (IntLiteral 69)
                it "parses 10000 as int" $
                    parseEither intLiteral "10000" `shouldBe` Right (IntLiteral 10000)
                it "parses 0 as int" $
                    parseEither intLiteral "0" `shouldBe` Right (IntLiteral 0)
                prop "parses int literals" $
                    \a ->
                        let n = abs a
                         in parseEither intLiteral (show n)
                                `shouldBe` Right (IntLiteral n)

            describe "bool literals" $ do
                it "parses 'false' as bool" $
                    parseEither boolLiteral "false" `shouldBe` Right (BoolLiteral False)
                it "parses 'true' as bool" $
                    parseEither boolLiteral "true" `shouldBe` Right (BoolLiteral True)

            describe "double literals" $ do
                it "parses 1 as double" $
                    parseEither doubleLiteral "1.0" `shouldBe` Right (DoubleLiteral 1)
                it "parses 69 as double" $
                    parseEither doubleLiteral "69.0" `shouldBe` Right (DoubleLiteral 69)
                it "parses 10000 as double" $
                    parseEither doubleLiteral "10000.0" `shouldBe` Right (DoubleLiteral 10000)
                it "parses 0 as double" $
                    parseEither doubleLiteral "0.0" `shouldBe` Right (DoubleLiteral 0)
                prop "parses double literals" $
                    \x ->
                        let positive = abs x
                         in parseEither doubleLiteral (T.pack $ printf "%f" positive)
                                `shouldBe` Right (DoubleLiteral positive)
            describe "operators" $ do
                it "parses adding 2 numbers" $
                    parseEither pExpr "0   + 6.9 " `shouldBe` Right (Addition (IntLiteral 0) (DoubleLiteral 6.9))
                it "parses comparing 2 numbers" $
                    parseEither pExpr "1>0" `shouldBe` Right (GreaterThen (IntLiteral 1) (IntLiteral 0))
                it "parses addition and multiplication precedence" $
                    parseEither pExpr "a   +2*3"
                        `shouldBe` Right
                            ( Addition
                                (Identifier "a")
                                ( Multiplication
                                    (IntLiteral 2)
                                    ( IntLiteral 3
                                    )
                                )
                            )
                it "parses overriding precedence with parens" $
                    parseEither pExpr "(a+2)*3"
                        `shouldBe` Right
                            ( Multiplication
                                ( Addition
                                    (Identifier "a")
                                    (IntLiteral 2)
                                )
                                (IntLiteral 3)
                            )
