module MiniLang (
    intLiteral,
    doubleLiteral,
) where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr
import Data.Text qualified as T
import Relude hiding (Sum)
import Relude.Unsafe as Unsafe
import Text.Megaparsec (ErrorItem (..), MonadParsec (notFollowedBy, try), ParseError, Parsec, failure, parse, satisfy, single, unexpected, (<?>), (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space, space1)
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
    = -- unary, right
      UnaryMinus Expression
    | BitwiseNeg Expression
    | LogicalNeg Expression
    | IntCast Expression
    | DoubleCast Expression
    | -- bitwise, left
      BitwiseSum Expression Expression
    | BitwiseMult Expression Expression
    | -- mulitiplicative, left
      Multiplication Expression Expression
    | Division Expression Expression
    | -- additive, left
      Sum Expression Expression
    | Subtraction Expression Expression
    | -- relations, left
      GreaterThen Expression Expression
    | GreaterThenEq Expression Expression
    | LessThen Expression Expression
    | LessThenEq Expression Expression
    | Equal Expression Expression
    | NotEqual Expression Expression
    | -- logical, left
      LogicOr Expression Expression
    | LogicAnd Expression Expression
    | -- assignment, right
      Assignment Identifier Expression
    | Identifier Identifier
    | IntLiteral Int
    | DoubleLiteral Double
    | BoolLiteral Bool
    deriving (Show)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = undefined

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

identifier :: Parser Expression
identifier = Identifier <$> (lexeme ident <?> "identifier")
  where
    ident = do
        firstChar :: Char <- letterChar
        rest <- many alphaNumChar
        return . T.pack $ firstChar : rest

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

between open close p = open *> p <* close

pTerm :: Parser Expression
pTerm =
    choice
        [ parens pExpr
        , identifier
        , try intLiteral
        , doubleLiteral
        ]

pExpr :: Parser Expression
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [
        [ prefix "-" UnaryMinus
        , prefix "~" BitwiseNeg
        , prefix "!" LogicalNeg
        , prefix "(int)" IntCast
        , prefix "(double)" DoubleCast
        ]
    ,
        [ binaryL "|" BitwiseSum
        , binaryL "&" BitwiseMult
        ]
    ,
        [ binaryL "*" Multiplication
        , binaryL "/" Division
        ]
    ,
        [ binaryL "+" Sum
        , binaryL "-" Subtraction
        ]
    ,
        [ binaryL ">" GreaterThen
        , binaryL ">=" GreaterThenEq
        , binaryL "<" LessThen
        , binaryL "<=" LessThenEq
        , binaryL "==" Equal
        , binaryL "!=" NotEqual
        ]
    ,
        [ binaryL "||" LogicOr
        , binaryL "&&" LogicAnd
        ]
    ,
        [ binaryR "=" Assignment
        ]
    ]

binaryR :: Text -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binaryR name f = InfixR (f <$ symbol name)

binaryL :: Text -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binaryL name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)

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
