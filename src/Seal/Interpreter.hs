module Seal.Interpreter (
    eval,
    evalExpression,
    Value (..),
    EvalError (..),
) where

import Control.Monad (foldM)
import Data.Bits (Bits ((.&.)), complement, (.|.))
import Data.HashMap.Strict qualified as HM
import Data.Text.IO (putStr)
import Relude hiding (putStr, putStrLn)
import Relude qualified as R
import Seal.Parser
import Text.Megaparsec (eof, parse)
import Text.Show (Show (..))

type ProgramState = Scope

data Error
    = EEval EvalError
    | EParse ParserError
    | ERead Expression
    | EType VarType Value
    | EAssignType Declaration Value
    | EUndeclared Identifier
    | ERedeclararion Identifier

instance Show Error where
    show (EEval e) = "eval error:" <> R.show e
    show (EParse e) = "parse error:" <> R.show e
    show (EType v t) = "expected type " <> R.show v <> ", got " <> R.show t
    show (EUndeclared ident) = "undeclared variable:" <> R.show ident
    show (ERedeclararion ident) = "redeclararion of variable:" <> R.show ident
    show (ERead expr) = "unexpected expression read from standard input" <> R.show expr
    show (EAssignType (name, type_) wrongValue) = "assigning " <> R.show wrongValue <> " to variable " <> R.show name <> " of type " <> R.show type_

eval :: Program -> IO (Either [Error] ())
eval (declarations, statements) = do
    let toValue (name, _type) = (name, Left _type)
        unassigned = HM.fromList (map toValue declarations)
    runProgram <- foldM doStatement ([], Just unassigned) statements
    return $ case runProgram of
        ([], _) -> Right ()
        (errors, _) -> Left errors
  where
    doStatement :: ([Error], Maybe ProgramState) -> Statement -> IO ([Error], Maybe ProgramState)
    doStatement (errors, Nothing) = const $ return (errors, Nothing)
    doStatement (errors, Just scope) = \case
        StBlock stmts -> foldM doStatement (errors, Just scope) stmts
        StExpression ex -> do
            case evalExpression scope ex of
                Right _ -> passArgsAsGiven
                Left err -> addError $ EEval err
        StIf condition statement ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    if cond
                        then doStatement (errors, Just scope) statement
                        else passArgsAsGiven
                Right wrongType -> addError $ EType TypeBool wrongType
                Left err -> addError $ EEval err
        StIfElse condition trueStatement falseStatement ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    doStatement (errors, Just scope) $
                        if cond then trueStatement else falseStatement
                Right wrongType -> addError $ EType TypeBool wrongType
                Left err -> addError $ EEval err
        while@(StWhile condition statement) ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    if cond
                        then do
                            newState <- doStatement (errors, Just scope) statement
                            doStatement newState while
                        else passArgsAsGiven
                Right wrongType -> addError $ EType TypeBool wrongType
                Left err -> addError $ EEval err
        StAssignment name value ->
            case HM.lookup name scope of
                Nothing -> addError $ EUndeclared name
                Just found ->
                    case evalExpression scope value of
                        (Right v) ->
                            case (found, v) of
                                (Left TypeInt, new@(VInt _)) -> assign new
                                (Right (VInt _), new@(VInt _)) -> assign new
                                (Left TypeBool, new@(VBool _)) -> assign new
                                (Right (VBool _), new@(VBool _)) -> assign new
                                (Left TypeDouble, new@(VDouble _)) -> assign new
                                (Right (VDouble _), new@(VDouble _)) -> assign new
                                (old, new) ->
                                    let err = EAssignType (name, toValue old) new
                                     in return (err : errors, Just scope)
                        Left err -> addError $ EEval err
                  where
                    toValue :: Either VarType Value -> VarType
                    toValue = \case
                        Left varType -> varType
                        Right (VInt _) -> TypeInt
                        Right (VBool _) -> TypeBool
                        Right (VDouble _) -> TypeDouble

                    assign v =
                        let newScope = HM.insert name (Right v) scope
                         in return (errors, Just newScope)
        StRead ident ->
            case HM.lookup ident scope of
                Nothing -> addError $ EUndeclared ident
                Just (Left type') -> case type' of
                    TypeInt -> parseAndInsert intLiteral VInt $ \case
                        IntLiteral n -> Right n
                        err -> Left $ ERead err
                    TypeBool -> parseAndInsert boolLiteral VBool $ \case
                        BoolLiteral v -> Right v
                        err -> Left $ ERead err
                    TypeDouble -> parseAndInsert doubleLiteral VDouble $ \case
                        DoubleLiteral v -> Right v
                        err -> Left $ ERead err
                  where
                    parseAndInsert literalParser constructor matcher = do
                        line <- getLine
                        case parse (literalParser <* eof) "" line of
                            Right r ->
                                case matcher r of
                                    Right value ->
                                        let newScope = HM.insert ident (Right (constructor value)) scope
                                         in return (errors, Just newScope)
                                    Left err -> addError err
                            Left parseError -> addError $ EParse parseError
                Just (Right existing) -> case existing of
                    VInt _ -> parseAndInsert intLiteral VInt $ \case
                        IntLiteral n -> Right n
                        err -> Left $ ERead err
                    VBool _ -> parseAndInsert boolLiteral VBool $ \case
                        BoolLiteral v -> Right v
                        err -> Left $ ERead err
                    VDouble _ -> parseAndInsert doubleLiteral VDouble $ \case
                        DoubleLiteral v -> Right v
                        err -> Left $ ERead err
                  where
                    parseAndInsert literalParser constructor matcher = do
                        line <- getLine
                        case parse (literalParser <* eof) "" line of
                            Right r ->
                                case matcher r of
                                    Right value ->
                                        let newScope = HM.insert ident (Right (constructor value)) scope
                                         in return (errors, Just newScope)
                                    Left err -> addError err
                            Left parseError -> addError $ EParse parseError
        StWriteExpr expr -> do
            case evalExpression scope expr of
                Right v -> do
                    putStr (justValue v)
                    passArgsAsGiven
                Left evalError -> addError (EEval evalError)
        StWriteText text -> putStr text >> passArgsAsGiven
        StReturn -> return (errors, Nothing)
        StDeclaration (ident, type_) -> do
            case HM.lookup ident scope of
                Nothing -> return (errors, Just $ HM.insert ident (Left type_) scope)
                Just _ -> addError (ERedeclararion ident)
      where
        passArgsAsGiven = return (errors, Just scope)
        addError err = return (err : errors, Just scope)

data EvalError
    = Binary Expression
    | Unary Value
    | NotInScope Identifier
    | Unassigned Identifier
    deriving (Show, Eq)

data Value = VInt Int | VDouble Double | VBool Bool
    deriving (Show, Eq)

justValue :: IsString s => Value -> s
justValue (VInt i) = Relude.show i
justValue (VDouble d) = Relude.show d
justValue (VBool b) = Relude.show b

showType :: IsString a => Value -> a
showType (VInt _) = "int"
showType (VDouble _) = "double"
showType (VBool _) = "bool"

type Scope = HM.HashMap Identifier (Either VarType Value)

-- TODO: better error handling
evalExpression :: Scope -> Expression -> Either EvalError Value
evalExpression scope = evalExpr
  where
    evalExpr :: Expression -> Either EvalError Value
    evalExpr = \case
        UnaryMinus e -> do
            innerExpr <- evalExpr e
            case innerExpr of
                VInt i -> return $ VInt (negate i)
                VDouble d -> return $ VDouble (negate d)
                err -> Left $ Unary err
        BitwiseNeg e ->
            evalExpr e >>= \case
                VInt i -> return $ VInt (complement i)
                err -> Left $ Unary err
        LogicalNeg e ->
            evalExpr e >>= \case
                VBool b -> return $ VBool (not b)
                err -> Left $ Unary err
        IntCast e ->
            evalExpr e >>= \case
                (VDouble d) -> return $ VInt (truncate d)
                (VBool b) -> return $ VInt $ if b then 1 else 0
                v@(VInt _) -> return v
        DoubleCast e ->
            evalExpr e >>= \case
                (VInt i) -> return $ VDouble (fromIntegral i)
                (VBool b) -> return $ VDouble $ if b then 1 else 0
                v@(VDouble _) -> return v
        BitwiseSum left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 .|. int2
                (_, _) -> Left $ Binary $ BitwiseSum left right
        BitwiseMult left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 .&. int2
                (_, _) -> Left $ Binary $ BitwiseMult left right
        Multiplication left right -> math Multiplication left right (*) (*)
        Division left right -> math Division left right div (/)
        Addition left right -> math Addition left right (+) (+)
        Subtraction left right -> math Subtraction left right (-) (-)
        GreaterThen left right -> comparison GreaterThen left right (>) (>)
        GreaterThenEq left right -> comparison GreaterThenEq left right (>=) (>=)
        LessThen left right -> comparison LessThen left right (<) (<)
        LessThenEq left right -> comparison LessThenEq left right (<=) (<=)
        Equal left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 == int2
                (VDouble d, VInt i) -> return $ VBool $ d == fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i == d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 == d2
                (VBool b1, VBool b2) -> return $ VBool $ b1 == b2
                (_, _) -> Left $ Binary $ Equal left right
        NotEqual left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 /= int2
                (VDouble d, VInt i) -> return $ VBool $ d /= fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i /= d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 /= d2
                (VBool b1, VBool b2) -> return $ VBool $ b1 /= b2
                (_, _) -> Left $ Binary $ NotEqual left right
        IntLiteral i -> return $ VInt i
        DoubleLiteral d -> return $ VDouble d
        BoolLiteral b -> return $ VBool b
        LogicOr left right -> do
            l <- evalExpr left
            if l == VBool True
                then -- return early, not sure if needed
                    return (VBool True)
                else do
                    r <- evalExpr right
                    case (l, r) of
                        (VBool b1, VBool b2) -> return $ VBool (b1 || b2)
                        (_, _) -> Left $ Binary $ LogicOr left right
        LogicAnd left right -> do
            l <- evalExpr left
            if l == VBool False
                then -- return early, not sure if needed
                    return (VBool False)
                else do
                    r <- evalExpr right
                    case (l, r) of
                        (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
                        (_, _) -> Left $ Binary $ LogicAnd left right
        Identifier ident ->
            case HM.lookup ident scope of
                Nothing -> Left $ NotInScope ident
                Just (Left _) -> Left $ Unassigned ident
                Just (Right v) -> return v
      where
        math operation left right opInt opDouble = do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 `opInt` int2
                (VDouble d, VInt i) -> return $ VDouble $ d `opDouble` fromIntegral i
                (VInt i, VDouble d) -> return $ VDouble $ fromIntegral i `opDouble` d
                (VDouble d1, VDouble d2) -> return $ VDouble $ d1 `opDouble` d2
                (_, _) -> Left $ Binary $ operation left right
        comparison operation left right opInt opDouble = do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 `opInt` int2
                (VDouble d, VInt i) -> return $ VBool $ d `opDouble` fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i `opDouble` d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 `opDouble` d2
                (_, _) -> Left $ Binary $ operation left right
