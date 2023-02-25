module MiniLang (
    intLiteral,
    doubleLiteral,
) where

import Control.Applicative hiding (many, some)
import Data.Text qualified as T
import Relude
import Relude.Unsafe as Unsafe
import Text.Megaparsec (ErrorItem (..), MonadParsec (notFollowedBy, try), ParseError, Parsec, failure, parse, satisfy, single, unexpected, (<?>), (<|>))
import Text.Megaparsec.Char (char, digitChar, space, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorItem (Tokens))

type Program = [Statement]

data Statement
    = StBlock [Statement]
    | StExpression Expression
    | StIf Expression [Statement]
    | StIfElse Expression [Statement] [Statement]
    | StWhile Expression [Statement]
    | StRead Identifier
    | StWriteExpr Expression
    | StWriteText Text
    | StReturn
    | StDeclaration Declaration
    deriving (Show)

type Identifier = Text

data Declaration
    = TypeBool Identifier
    | TypeInt Identifier
    | TypeDouble Identifier
    deriving (Show)

data Expression
    = -- TODO: precedence here or somewhere else?
      UnaryOp UnaryOperator Expression -- right
    | BitwiseOp Expression BitwiseOperator Expression -- left
    | MultiplicationOp Expression BinaryOperator Expression -- left
    | AdditiveOp Expression BinaryOperator Expression -- left
    | RelationOp Expression BinaryOperator Expression -- left
    | LogicOp Expression BinaryOperator Expression -- left
    | Assignment Identifier Expression -- right
    | Identifier Identifier
    | IntLiteral Int
    | DoubleLiteral Double
    | BoolLiteral Bool
    deriving (Show)

type UnaryOperator = Undefined
type BitwiseOperator = Undefined
type BinaryOperator = Undefined

type Parser = Parsec Void Text

intLiteral :: Parser Expression
intLiteral = (try zeroParser <|> otherNumberParser) <?> "integer"
  where
    zeroParser :: Parser Expression
    zeroParser = do
        zero <- single '0'
        notFollowedBy otherNumberParser
        toNumber [zero]

    otherNumberParser :: Parser Expression
    otherNumberParser = do
        firstDigit <- satisfy (`elem` ['1' .. '9'])
        rest <- some digitChar
        toNumber $ firstDigit : rest

    toNumber :: [Char] -> Parser Expression
    toNumber = return . IntLiteral . Unsafe.read

doubleLiteral :: Parser Expression
doubleLiteral = do
    beforeDot <- try zeroParser <|> otherNumberParser
    _ <- single '.'
    afterDot <- some digitChar
    toNumber (beforeDot ++ "." ++ afterDot) <?> "double"
  where
    zeroParser :: Parser String
    zeroParser = do
        zero <- single '0'
        notFollowedBy otherNumberParser
        return [zero]

    otherNumberParser :: Parser String
    otherNumberParser = do
        firstDigit <- satisfy (`elem` ['1' .. '9'])
        rest <- some digitChar
        return $ firstDigit : rest

    toNumber :: [Char] -> Parser Expression
    toNumber = return . DoubleLiteral . Unsafe.read

-- Elementami podstawowej wersji języka Mini są następujące terminale:
-- - słowa kluczowe: program if else while read write return int double bool true false
-- - operatory i symbole specjalne: = || && | & == != > >= < <= + - * / ! ~ ( ) { } ;
-- - identyfikatory i liczby
