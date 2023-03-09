{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module MiniLang (
    Declaration (..),
    Expression (..),
    Parser,
    ParserError,
    Program,
    Statement (..),
    bitwise,
    stIf,
    boolLiteral,
    declaration,
    doubleLiteral,
    identifierName,
    intLiteral,
    multiplicative,
    pExpr,
    pTerm,
    parseMiniLang,
    programParser,
    readP,
    returnP,
    spaceConsumer,
    stExpression,
    unaryOp,
    writeExpr,
    writeText,
) where

import Data.Text qualified as T
import Relude hiding (Sum, many, some)
import Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

type Program = ([Declaration], [Statement])

data Statement
    = StBlock [Statement]
    | StExpression Expression
    | StIf Expression Statement
    | StIfElse Expression Statement Statement
    | StWhile Expression Statement
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
      Addition Expression Expression
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
            [ hidden space1
            , hidden comment
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

programParser :: Parser ([Declaration], [Statement])
programParser = do
    spaceConsumer
    _ <- symbol "program"
    _ <- symbol "{"
    declarations <- many declaration
    statements <- many statement
    _ <- symbol "}"
    eof
    return (declarations, statements)

identifier :: Parser Expression
identifier = Identifier <$> identifierName

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- FIXME: precedence
pExpr :: Parser Expression
pExpr = do
    firstTerm <- pTerm
    rest <- many ops
    return $ foldl' (&) firstTerm rest
  where
    ops :: Parser (Expression -> Expression)
    ops =
        choice
            [ bitwise
            , multiplicative
            , additive
            ]

pTerm :: Parser Expression
pTerm =
    lexeme $
        choice
            [ try unaryOp
            , boolLiteral
            , try doubleLiteral
            , intLiteral
            , identifier
            , parens pExpr
            ]

unaryOp :: Parser Expression
unaryOp = do
    operators <- some allOperatos
    operand <- pTerm
    return $ foldr ($) operand operators
  where
    allOperatos =
        choice
            [ op "-" UnaryMinus
            , op "~" BitwiseNeg
            , op "!" LogicalNeg
            , try $ cast "int" IntCast
            , try $ cast "double" DoubleCast
            ]
    op sign constructor = symbol sign $> constructor
    cast typeTo constructor = parens (symbol typeTo) $> constructor

bitwise :: Parser (Expression -> Expression)
bitwise = leftAssociative operators pTerm
  where
    operators :: Parser (Expression -> Expression -> Expression)
    operators =
        choice
            [ symbol "|" $> BitwiseSum
            , symbol "&" $> BitwiseMult
            ]

rightAssociative :: Parser (Expression -> Expression -> Expression) -> Parser Expression -> Parser Expression
rightAssociative ops termP = do
    lastTerm <- termP
    leftSide <- many termAndOp
    return $
        foldr ($) lastTerm leftSide
  where
    termAndOp :: Parser (Expression -> Expression)
    termAndOp = do
        leftSide <- undefined -- TODO: recursion
        operator <- ops
        return $ \r -> leftSide `operator` r

-- leftAssociative :: Parser (Expression -> Expression -> Expression) -> Parser Expression -> Parser Expression
-- leftAssociative ops termP = do
--     -- leftSide <- termP
--     rightSide <- many opAndTerm
--     return $
--         foldl'
--             (\termLeft (op, termRight) -> termLeft `op` termRight)
--             leftSide
--             rightSide
--   where
--     opAndTerm = do
--         operator <- ops
--         rightSide <- termP
--         return (operator, rightSide)

leftAssociative :: Parser (Expression -> Expression -> Expression) -> Parser Expression -> Parser (Expression -> Expression)
leftAssociative ops termP = do
    operator <- ops
    rightSide <- termP
    return (`operator` rightSide)

-- ops :: Parser (Expression -> Expression -> Expression)
-- ops =
--     choice
--         [ symbol "*" $> Multiplication
--         , symbol "/" $> Division
--         ]

multiplicative :: Parser (Expression -> Expression)
multiplicative = leftAssociative ops pTerm
  where
    ops :: Parser (Expression -> Expression -> Expression)
    ops =
        choice
            [ symbol "*" $> Multiplication
            , symbol "/" $> Division
            ]
additive :: Parser (Expression -> Expression)
additive = leftAssociative ops pTerm
  where
    ops :: Parser (Expression -> Expression -> Expression)
    ops =
        choice
            [ symbol "+" $> Addition
            , symbol "-" $> Subtraction
            ]

boolLiteral :: Parser Expression
boolLiteral =
    lexeme
        ( choice
            [ string "true" $> BoolLiteral True
            , string "false" $> BoolLiteral False
            ]
            <?> "bool"
        )

type ParserError = ParseErrorBundle Text Void

parseMiniLang :: Text -> Either ParserError ([Declaration], [Statement])
parseMiniLang = runParser programParser "input"

writeExpr :: Parser Statement
writeExpr = do
    keyword "write"
    expression <- pExpr
    keyword ";"
    return $ StWriteExpr expression

writeText :: Parser Statement
writeText = do
    keyword "write"
    text <- stringLiteral
    keyword ";"
    return $ StWriteText text

stringLiteral :: Parser Text
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack

readP :: Parser Statement
readP = do
    keyword "read"
    ident <- identifierName
    keyword ";"
    return $ StRead ident

returnP :: Parser Statement
returnP = keyword "return" *> keyword ";" $> StReturn

stExpression :: Parser Statement
stExpression = do
    expr <- pExpr
    keyword ";"
    return $ StExpression expr

stBlock :: Parser Statement
stBlock = do
    keyword "{"
    st <- many statement
    keyword "}"
    return $ StBlock st

stDeclaration :: Parser Statement
stDeclaration = StDeclaration <$> declaration

statement :: Parser Statement
statement =
    choice
        [ stBlock
        , try writeText
        , writeExpr
        , stIf
        , stWhile
        , readP
        , returnP
        , stDeclaration
        , stExpression
        ]

stWhile :: Parser Statement
stWhile = do
    keyword "while"
    condition <- parens pExpr
    StWhile condition <$> statement

stIf :: Parser Statement
stIf = do
    keyword "if"
    condition <- parens pExpr
    StIf condition <$> statement

-- "*" Multiplication
-- "/" Division
--
-- "+" Addition
-- "-" Subtraction
--
-- ">" GreaterThen
-- ">=" GreaterThenEq
-- "<" LessThen
-- "<=" LessThenEq
-- "==" Equal
-- "!=" NotEqual
--
-- "||" LogicOr
-- "&&" LogicAnd

intLiteral :: Parser Expression
intLiteral = do
    intString <- intParser
    lexeme (toNumber IntLiteral intString)

doubleLiteral :: Parser Expression
doubleLiteral = do
    beforeDot <- intParser
    _ <- single '.'
    afterDot <- some digitChar
    lexeme $ label "double" $ toNumber DoubleLiteral (beforeDot ++ "." ++ afterDot)

intParser :: Parser String
intParser = label "number" $ try zeroParser <|> otherNumberParser
  where
    zeroParser :: Parser String
    zeroParser = do
        zero <- single '0'
        notFollowedBy otherNumberParser
        return [zero]

    otherNumberParser :: Parser String
    otherNumberParser = do
        firstDigit <- satisfy (`elem` ['1' .. '9'])
        rest <- many digitChar
        return $ firstDigit : rest

toNumber :: (Read n) => (n -> Expression) -> String -> Parser Expression
toNumber constructor = return . constructor . Unsafe.read

declaration :: Parser Declaration
declaration =
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
