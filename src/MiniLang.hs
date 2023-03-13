{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module MiniLang (
    Declaration (..),
    Expression (..),
    Parser,
    ParserError,
    Program,
    Statement (..),
    stIf,
    boolLiteral,
    declaration,
    doubleLiteral,
    identifierName,
    intLiteral,
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
    expression,
) where

import Data.Text qualified as T
import Relude hiding (Sum, many, some)
import Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

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

expression :: Parser Expression
expression = binaries
  where
    binaries = foldr binOp value operatorPrecedence
    operatorPrecedence =
        -- from lowest to highest precedence
        [
            [ symbol "||" $> LogicOr
            , symbol "&&" $> LogicAnd
            ]
        ,
            [ symbol ">" $> GreaterThen
            , symbol ">=" $> GreaterThenEq
            , symbol "<" $> LessThen
            , symbol "<=" $> LessThenEq
            , symbol "==" $> Equal
            , symbol "!=" $> NotEqual
            ]
        ,
            [ symbol "+" $> Addition
            , symbol "-" $> Subtraction
            ]
        ,
            [ symbol "*" $> Multiplication
            , symbol "/" $> Division
            ]
        ,
            [ single' '|' $> BitwiseSum
            , single' '&' $> BitwiseMult
            ]
        ]
    single' c = lexeme $ try $ char c *> notFollowedBy (char c)
    value = pTerm
    binOp ::
        [Parser (Expression -> Expression -> Expression)] ->
        Parser Expression ->
        Parser Expression
    binOp operators higherPrec = do
        left <- higherPrec
        right <- many rest
        return $ foldl' (\l (op, r) -> l `op` r) left right
      where
        rest = do
            op <- choice operators
            right <- higherPrec
            return (op, right)

pTerm :: Parser Expression
pTerm =
    lexeme $
        choice $
            concat
                [ [try unaryOp]
                , terminals
                , [parens expression]
                ]

terminals :: [Parser Expression]
terminals =
    [ boolLiteral
    , try doubleLiteral
    , intLiteral
    , identifier
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
    expr <- expression
    keyword ";"
    return $ StWriteExpr expr

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
    expr <- expression
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
    condition <- parens expression
    StWhile condition <$> statement

stIf :: Parser Statement
stIf = do
    keyword "if"
    condition <- parens expression
    st <- statement
    elseSt <- optional elseStatement
    return $ case elseSt of
        Just el -> StIfElse condition st el
        Nothing -> StIf condition st
  where
    elseStatement :: Parser Statement
    elseStatement = keyword "else" *> statement

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
