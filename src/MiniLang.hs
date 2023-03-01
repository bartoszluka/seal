module MiniLang (
    intLiteral,
    doubleLiteral,
    parseMiniLang,
    boolLiteral,
    spaceConsumer,
    declarationParser,
    Declaration (..),
    identifierName,
) where

import Control.Monad.Combinators (choice)
import Control.Monad.Combinators.Expr
import Data.Text qualified as T
import Relude hiding (Sum, many, some)
import Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, digitChar, letterChar, newline, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type Program = ([Declaration], [Statement])

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
    deriving (Eq, Show)

type Identifier = Text

data Declaration
    = TypeBool Identifier
    | TypeInt Identifier
    | TypeDouble Identifier
    deriving (Eq, Show)

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
    deriving (Eq, Show)

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
    skipMany $
        choice
            [ space1
            , comment
            ]
  where
    comment :: Parser ()
    comment = do
        _ <- string "//"
        _ <- skipManyTill anySingle "\n"
        return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

programParser :: Parser ()
programParser = do
    spaceConsumer
    _ <- symbol "program"
    _ <- symbol "{"
    _ <- symbol "}"
    return ()

identifierP :: Parser Expression
identifierP = Identifier <$> (lexeme ident <?> "identifier")
  where
    ident = do
        firstChar :: Char <- letterChar
        rest <- many alphaNumChar
        return . T.pack $ firstChar : rest

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- between :: Applicative f => f a1 -> f b -> f a2 -> f a2
-- between open close p = open *> p <* close
--
pTerm :: Parser Expression
pTerm =
    choice
        [ parens pExpr
        , identifierP
        , try intLiteral
        , doubleLiteral
        , boolLiteral
        ]

boolLiteral :: Parser Expression
boolLiteral =
    (string "true" $> BoolLiteral True)
        <|> (string "false" $> BoolLiteral False)

pExpr :: Parser Expression
pExpr = makeExprParser pTerm operatorTable

parseMiniLang :: Text -> Either (ParseErrorBundle Text Void) ()
parseMiniLang = runParser programParser "input"

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
        -- ,
        --     [ binaryR "=" Assignment
        --     ]
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

declarationParser :: Parser Declaration
declarationParser =
    choice
        [ keyword "bool" *> (TypeBool <$> identifierName)
        , keyword "int" *> (TypeInt <$> identifierName)
        , keyword "double" *> (TypeDouble <$> identifierName)
        ]
        <* symbol ";"

identifierName :: Parser Identifier
identifierName = lexeme ident <?> "identifier"
  where
    ident = do
        firstChar :: Char <- letterChar
        rest <- many alphaNumChar
        return . T.pack $ firstChar : rest

keyword :: Text -> Parser ()
keyword kw = symbol kw $> ()

-- Elementami podstawowej wersji języka Mini są następujące terminale:
-- - słowa kluczowe: program if else while read write return int double bool true false
-- - operatory i symbole specjalne: = || && | & == != > >= < <= + - * / ! ~ ( ) { } ;
-- - identyfikatory i liczby
