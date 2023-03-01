{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Relude

import Test.Hspec
import Text.RawString.QQ

-- import Test.QuickCheck

import MiniLang (Declaration (..), declarationParser, identifierName, parseMiniLang)
import Text.Megaparsec (parse, parseMaybe)

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

        describe "identifiers" $ do
            it "parses identifiers with only letters" $ do
                parse identifierName "" "abcd" `shouldBe` Right "abcd"
                parse identifierName "" "AbCd" `shouldBe` Right "AbCd"
            it "parses identifiers with letters and numbers" $ do
                parse identifierName "" "abcd2df3jp2gm" `shouldBe` Right "abcd2df3jp2gm"
                parse identifierName "" "ABCeJP2gmD" `shouldBe` Right "ABCeJP2gmD"
            it "does not parse identifiers with '_' (underscore)" $ do
                parse identifierName "" "AbCd_234" `shouldNotBe` Right "AbCd_234"
                parse identifierName "" "_AbCd_234" `shouldNotBe` Right "_AbCd_234"
            it "does not parse identifiers that start with numbers" $ do
                parse identifierName "" "1AbCd234" `shouldSatisfy` isLeft
                parse identifierName "" "2344AbCd234" `shouldSatisfy` isLeft

        describe "declarations" $ do
            it "parses declaration of an int variable" $
                parseMaybe declarationParser "int i;" `shouldBe` Just (TypeInt "i")
            it "parses declaration of an int variable with different ammount of whitespace" $ do
                parseMaybe declarationParser "int       i  ;" `shouldBe` Just (TypeInt "i")
                parse
                    declarationParser
                    ""
                    [r|int     
                                    abcd123
                                    
                                    ;|]
                    `shouldBe` Right (TypeInt "abcd123")
            it "parses declaration of a 'double' variable" $
                parseMaybe declarationParser "double i;" `shouldBe` Just (TypeDouble "i")
            it "parses declaration of a 'bool' variable" $
                parseMaybe declarationParser "bool i;" `shouldBe` Just (TypeBool "i")
