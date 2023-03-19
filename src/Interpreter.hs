{-# LANGUAGE LambdaCase #-}

module Interpreter (eval, evalExpression, Value (..), EvalError (..)) where

import Control.Monad (foldM, foldM_)
import Data.Bits (Bits ((.&.)), complement, (.|.))
import Data.HashMap.Strict qualified as HM
import MiniLang
import Relude
import Text.Megaparsec (eof, parse)

type ProgramState = Scope

eval :: Program -> IO ()
eval (declarations, statements) = do
    let unassigned = HM.fromList (map toValue declarations)
    foldM_ doStatement unassigned statements
  where
    toValue (name, _type) = (name, Nothing)
    doStatement :: ProgramState -> Statement -> IO ProgramState
    doStatement scope = \case
        StBlock stmts -> foldM doStatement scope stmts
        StExpression ex -> do
            case evalExpression scope ex of
                Right _ -> return scope
                Left err -> do
                    -- TODO: accumulate errors
                    print err
                    return scope
        StIf condition statement ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    if cond
                        then doStatement scope statement
                        else return scope
                Right _ -> do
                    print "condition must be a boolean"
                    return scope
                Left err -> do
                    -- TODO: accumulate errors
                    print err
                    return scope
        StIfElse condition trueStatement falseStatement ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    doStatement scope $
                        if cond then trueStatement else falseStatement
                Right _ -> do
                    print "condition must be a boolean"
                    return scope
                Left err -> do
                    -- TODO: accumulate errors
                    print err
                    return scope
        while@(StWhile condition statement) ->
            case evalExpression scope condition of
                Right (VBool cond) ->
                    if cond
                        then do
                            newState <- doStatement scope statement
                            doStatement newState while
                        else return scope
                Right _ -> do
                    print "condition must be a boolean"
                    return scope
                Left err -> do
                    -- TODO: accumulate errors
                    print err
                    return scope
        StAssignment name value ->
            case HM.lookup name scope of
                Nothing -> do
                    -- TODO: errors
                    print $ "undeclared " <> name
                    return scope
                Just _ ->
                    case evalExpression scope value of
                        Right v ->
                            do
                                -- TODO: types!
                                let newScope = HM.insert name (Just v) scope
                                return newScope
                        Left err -> do
                            -- TODO: accumulate errors
                            print err
                            return scope
        StRead ident ->
            case HM.lookup ident scope of
                Nothing -> do
                    -- TODO: errors
                    print $ "undeclared " <> ident
                    return scope
                -- TODO: types!
                Just _ -> do
                    line <- getLine

                    case parse (expression <* eof) "" line of
                        Right expr -> case evalExpression scope expr of
                            Right v ->
                                let newScope = HM.insert ident (Just v) scope
                                 in return newScope
                            Left evalError -> do
                                -- TODO: errors
                                print evalError
                                return scope
                        Left parseError -> do
                            -- TODO: errors
                            print parseError
                            return scope
        StWriteExpr expr -> do
            case evalExpression scope expr of
                Right v -> do
                    putStrLn (justValue v)
                    return scope
                Left err -> do
                    -- TODO:
                    print err
                    return scope
        StWriteText text -> print text >> return scope
        StReturn -> return scope
        StDeclaration (ident, _type) -> do
            case HM.lookup ident scope of
                Nothing -> return (HM.insert ident Nothing scope)
                Just _ -> do
                    print $ "redeclaration of " <> ident
                    return scope

data EvalError
    = Binary (Value, Value)
    | Unary Value
    | Expr Expression
    | NotInScope Identifier
    | Unassigned Identifier
    deriving (Show, Eq)

data Value = VInt Int | VDouble Double | VBool Bool
    deriving (Show, Eq)

justValue (VInt i) = show i
justValue (VDouble d) = show d
justValue (VBool b) = show b


showType :: IsString a => Value -> a
showType (VInt _) = "int"
showType (VDouble _) = "double"
showType (VBool _) = "bool"

type Scope = HM.HashMap Identifier (Maybe Value)

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
                (b1, b2) -> Left $ Binary (b1, b2)
        BitwiseMult left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 .&. int2
                (b1, b2) -> Left $ Binary (b1, b2)
        Multiplication left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 * int2
                (VDouble d, VInt i) -> return $ VDouble $ d * fromIntegral i
                (VInt i, VDouble d) -> return $ VDouble $ fromIntegral i * d
                (VDouble d1, VDouble d2) -> return $ VDouble $ d1 * d2
                (b1, b2) -> Left $ Binary (b1, b2)
        Division left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 `div` int2
                (VDouble d, VInt i) -> return $ VDouble $ d / fromIntegral i
                (VInt i, VDouble d) -> return $ VDouble $ fromIntegral i / d
                (VDouble d1, VDouble d2) -> return $ VDouble $ d1 / d2
                (b1, b2) -> Left $ Binary (b1, b2)
        Addition left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 + int2
                (VDouble d, VInt i) -> return $ VDouble $ d + fromIntegral i
                (VInt i, VDouble d) -> return $ VDouble $ fromIntegral i + d
                (VDouble d1, VDouble d2) -> return $ VDouble $ d1 + d2
                (b1, b2) -> Left $ Binary (b1, b2)
        Subtraction left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VInt $ int1 - int2
                (VDouble d, VInt i) -> return $ VDouble $ d - fromIntegral i
                (VInt i, VDouble d) -> return $ VDouble $ fromIntegral i - d
                (VDouble d1, VDouble d2) -> return $ VDouble $ d1 - d2
                (b1, b2) -> Left $ Binary (b1, b2)
        GreaterThen left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 > int2
                (VDouble d, VInt i) -> return $ VBool $ d > fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i > d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 > d2
                (b1, b2) -> Left $ Binary (b1, b2)
        GreaterThenEq left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 >= int2
                (VDouble d, VInt i) -> return $ VBool $ d >= fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i >= d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 >= d2
                (b1, b2) -> Left $ Binary (b1, b2)
        LessThen left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 < int2
                (VDouble d, VInt i) -> return $ VBool $ d < fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i < d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 < d2
                (b1, b2) -> Left $ Binary (b1, b2)
        LessThenEq left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 <= int2
                (VDouble d, VInt i) -> return $ VBool $ d <= fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i <= d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 <= d2
                (b1, b2) -> Left $ Binary (b1, b2)
        Equal left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 == int2
                (VDouble d, VInt i) -> return $ VBool $ d == fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i == d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 == d2
                (VBool b1, VBool b2) -> return $ VBool $ b1 == b2
                (v1, v2) -> Left $ Binary (v1, v2)
        NotEqual left right -> do
            l <- evalExpr left
            r <- evalExpr right
            case (l, r) of
                (VInt int1, VInt int2) -> return $ VBool $ int1 /= int2
                (VDouble d, VInt i) -> return $ VBool $ d /= fromIntegral i
                (VInt i, VDouble d) -> return $ VBool $ fromIntegral i /= d
                (VDouble d1, VDouble d2) -> return $ VBool $ d1 /= d2
                (VBool b1, VBool b2) -> return $ VBool $ b1 /= b2
                (v1, v2) -> Left $ Binary (v1, v2)
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
                        (v1, v2) -> Left $ Binary (v1, v2)
        LogicAnd left right -> do
            l <- evalExpr left
            if l == VBool False
                then -- return early, not sure if needed
                    return (VBool False)
                else do
                    r <- evalExpr right
                    case (l, r) of
                        (VBool b1, VBool b2) -> return $ VBool (b1 && b2)
                        (v1, v2) -> Left $ Binary (v1, v2)
        Identifier ident ->
            case HM.lookup ident scope of
                Nothing -> Left $ NotInScope ident
                Just Nothing -> Left $ Unassigned ident
                Just (Just v) -> return v
