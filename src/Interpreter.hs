{-# LANGUAGE LambdaCase #-}

module Interpreter (eval, evalExpression, Value (..), EvalError (..)) where

import Data.Bits (Bits ((.&.)), complement, (.|.))
import Data.HashMap.Strict qualified as HM
import MiniLang
import Relude

eval :: Program -> IO ()
eval = undefined

data EvalError
    = Binary (Value, Value)
    | Unary Value
    | Expr Expression
    | NotInScope Identifier
    deriving (Show, Eq)

data Value = VInt Int | VDouble Double | VBool Bool
    deriving (Show, Eq)

showType :: IsString a => Value -> a
showType (VInt _) = "int"
showType (VDouble _) = "double"
showType (VBool _) = "bool"

type Scope = HM.HashMap Identifier Value

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
                Just v -> return v

--
