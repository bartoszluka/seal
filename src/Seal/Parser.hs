{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Seal.Parser (
    Declaration,
    Expression (..),
    FunctionDefinition (..),
    Identifier,
    LetBinding (..),
    Parser,
    ParserError,
    Program,
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
    programParser,
    spaceConsumer,
    stExpression,
    stIf,
    stRead,
    stReturn,
    stWriteExpr,
    stWriteText,
    term,
    unaryOp,
) where

import Data.Char (isUpper)
import Data.Text qualified as T
import Relude hiding (Sum, many, some)
import Relude.Unsafe as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Program = [FunctionDefinition]

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
    | StAssignment Identifier Expression
    deriving (Eq, Show)

type Identifier = Text

type Declaration = (Identifier, VarType)

data FunctionDefinition = FunctionDefinition
    { functionName :: Identifier
    , functionArgs :: [(Identifier, Maybe TypeName)]
    , functionReturnType :: Maybe TypeName
    , functionBody :: Expression
    }
    deriving (Eq, Show)

data VarType = TypeBool | TypeInt | TypeDouble | TypeFunction FunctionDefinition deriving (Eq, Show)

typeBool :: Identifier -> Declaration
typeBool ident = (ident, TypeBool)

typeInt :: Identifier -> Declaration
typeInt ident = (ident, TypeInt)

typeDouble :: Identifier -> Declaration
typeDouble ident = (ident, TypeDouble)

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
    | Block [LetBinding] Expression
    | FunctionCall Identifier [Expression]
    deriving (Eq, Show)

data LetBinding = LetBinding Identifier Expression
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

programParser :: Parser Program
programParser = do
    spaceConsumer
    functions <- many functionDefinition
    eof
    return functions

functionDefinition :: Parser FunctionDefinition
functionDefinition = do
    keyword "fn"
    name <- identifierName
    args <- parens (sepEndBy argument (symbol ","))
    returnType <- optional typeAnnotation
    _ <- symbol "="
    body <- expression

    return
        FunctionDefinition
            { functionName = name
            , functionArgs = args
            , functionReturnType = returnType
            , functionBody = body
            }
  where
    typeAnnotation = symbol ":" >> typeName

    argument :: Parser (Identifier, Maybe TypeName)
    argument = do
        name <- identifierName
        typ <- optional typeAnnotation
        return (name, typ)

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
    value = term
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

blockExpression :: Parser Expression
blockExpression = do
    _ <- symbol "{"
    letBindings <- many letBinding
    expr <- expression
    _ <- symbol "}"
    return $ Block letBindings expr
  where
    letBinding :: Parser LetBinding
    letBinding = do
        keyword "let"
        lhs <- identifierName
        _ <- symbol "="
        rhs <- expression
        return $ LetBinding lhs rhs

functionCall :: Parser Expression
functionCall = do
    functionName' <- identifierName
    args <- parens $ sepEndBy1 expression (symbol ",")
    return $ FunctionCall functionName' args

term :: Parser Expression
term =
    lexeme $
        choice
            [ try unaryOp
            , boolLiteral
            , try doubleLiteral
            , intLiteral
            , try identifier
            , try functionCall
            , blockExpression
            , parens expression
            ]

unaryOp :: Parser Expression
unaryOp = do
    operators <- some allOperatos
    operand <- term
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

parseFile :: Text -> Either ParserError Program
parseFile = runParser programParser "input"

stWriteExpr :: Parser Statement
stWriteExpr = do
    keyword "write"
    expr <- expression
    keyword ";"
    return $ StWriteExpr expr

stWriteText :: Parser Statement
stWriteText = do
    keyword "write"
    text <- stringLiteral
    keyword ";"
    return $ StWriteText text

stringLiteral :: Parser Text
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack

stRead :: Parser Statement
stRead = do
    keyword "read"
    ident <- identifierName
    keyword ";"
    return $ StRead ident

stReturn :: Parser Statement
stReturn = keyword "return" *> keyword ";" $> StReturn

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
        , try stWriteText
        , stWriteExpr
        , stIf
        , stWhile
        , stRead
        , stReturn
        , stDeclaration
        , try stAssignment
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
        Just else_ -> StIfElse condition st else_
        Nothing -> StIf condition st
  where
    elseStatement :: Parser Statement
    elseStatement = keyword "else" *> statement

stAssignment :: Parser Statement
stAssignment = do
    name <- identifierName
    keyword "="
    value <- expression
    keyword ";"
    return $ StAssignment name value
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
        [ keyword "bool" *> (typeBool <$> identifierName)
        , keyword "int" *> (typeInt <$> identifierName)
        , keyword "double" *> (typeDouble <$> identifierName)
        ]
        <* symbol ";"

type TypeName = Identifier
typeName :: Parser TypeName
typeName = lexeme ident <?> "type"
  where
    ident = do
        firstChar <- capitalLetter
        rest <- many alphaNumChar
        return . T.pack $ firstChar : rest

    capitalLetter :: Parser Char
    capitalLetter = satisfy isUpper

-- BUG: keywords can be identifier names
identifierName :: Parser Identifier
identifierName = lexeme ident <?> "identifier"
  where
    ident = do
        firstChar :: Char <- letterChar
        rest <- many alphaNumChar
        return . T.pack $ firstChar : rest

keyword :: Text -> Parser ()
keyword kw = symbol kw $> ()
