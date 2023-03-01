{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Relude

import Test.Hspec
import Text.RawString.QQ

-- import Test.QuickCheck

import MiniLang (Declaration (..), Parser, ParserError, declaration, identifierName, parseMiniLang)
import Text.Megaparsec (eof, parse, parseMaybe, parseTest)

parseEither :: Parser a -> Text -> Either ParserError a
parseEither parser = parse (parser <* eof) ""

main :: IO ()
main = hspec $ do
    describe "parsing" $ do
        describe "program" $ do
            it "parses program with no statements" $
                parseMiniLang "program{}" `shouldSatisfy` isRight
            it "parses program with no statements and some whitespace" $ do
                parseMiniLang "program  {  }" `shouldSatisfy` isRight
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

        describe "identifiers" $ do
            it "parses identifiers with only letters" $ do
                parseEither identifierName "abcd" `shouldBe` Right "abcd"
                parseEither identifierName "AbCd" `shouldBe` Right "AbCd"
            it "parses identifiers with letters and numbers" $ do
                parseEither identifierName "abcd2df3jp2gm" `shouldBe` Right "abcd2df3jp2gm"
                parseEither identifierName "ABCeJP2gmD" `shouldBe` Right "ABCeJP2gmD"
            it "does not parse identifiers with '_' (underscore)" $ do
                parseEither identifierName "AbCd_234" `shouldNotBe` Right "AbCd_234"
                parseEither identifierName "_AbCd_234" `shouldNotBe` Right "_AbCd_234"
            it "does not parse identifiers that start with numbers" $ do
                parseEither identifierName "1AbCd234" `shouldSatisfy` isLeft
                parseTest identifierName "1AbCd234" `shouldSatisfy` isRight
                parseEither identifierName "2344AbCd234" `shouldSatisfy` isLeft

        describe "declarations" $ do
            it "parses declaration of an int variable" $
                parseMaybe declaration "int i;" `shouldBe` Just (TypeInt "i")
            it "parses declaration of an int variable with different ammount of whitespace" $ do
                parseMaybe declaration "int       i  ;" `shouldBe` Just (TypeInt "i")
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
