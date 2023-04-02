{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seal.Compiler (typecheck, Error (..), codegen) where

import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Relude
import Seal.Interpreter (EvalError, Value (..), evalExpression)
import Seal.Parser (Declaration, Expression (..), Identifier, Program, Statement (..), VarType (..))

data Error
    = EEval EvalError
    | EType VarType Value
    | EAssignType Declaration Value
    | EUndeclared Identifier
    | ERedeclararion Identifier
    deriving (Show, Eq)

type Scope = HM.HashMap Identifier (Either VarType Value)

typecheck :: Program -> Either (NonEmpty Error) ()
typecheck (declarations, statements) =
    let toValue (name, _type) = (name, Left _type)
        unassigned :: Scope
        unassigned = HM.fromList (map toValue declarations)
     in evalState (errorOnNonEmpty <$> mapM checkStatement statements) unassigned
  where
    errorOnNonEmpty eithers = case lefts eithers of
        [] -> Right ()
        x : xs -> Left $ foldl' (<>) x xs
    checkStatement :: Statement -> State Scope (Either (NonEmpty Error) ())
    checkStatement = \case
        StBlock stmts -> do
            scope <- get
            evalState (return . errorOnNonEmpty <$> mapM checkStatement stmts) scope
        StIf condition _ -> do
            scope <- get
            case evalExpression scope condition of
                Right (VBool _) -> ok
                Right wrongType -> err $ EType TypeBool wrongType
                Left e -> err $ EEval e
        StAssignment name value -> do
            scope <- get
            case HM.lookup name scope of
                Nothing -> err $ EUndeclared name
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
                                (old, new) -> err $ EAssignType (name, toValue old) new
                        Left e -> err $ EEval e
                  where
                    toValue :: Either VarType Value -> VarType
                    toValue = \case
                        Left varType -> varType
                        Right (VInt _) -> TypeInt
                        Right (VBool _) -> TypeBool
                        Right (VDouble _) -> TypeDouble

                    assign :: Value -> State Scope (Either a ())
                    assign newValue = fmap Right $ modify $ HM.insert name (Right newValue)
        StExpression ex -> do
            scope <- get
            case evalExpression scope ex of
                Right _ -> ok
                Left e -> err $ EEval e
        StIfElse condition _ _ -> do
            scope <- get
            case evalExpression scope condition of
                Right (VBool _) -> ok
                Right wrongType -> err $ EType TypeBool wrongType
                Left e -> err $ EEval e
        (StWhile condition _) -> do
            scope <- get
            case evalExpression scope condition of
                Right (VBool _) -> ok
                Right wrongType -> err $ EType TypeBool wrongType
                Left e -> err $ EEval e
        StRead ident -> do
            scope <- get
            case HM.lookup ident scope of
                Nothing -> err $ EUndeclared ident
                Just _ -> ok
        StWriteExpr expr -> do
            scope <- get
            case evalExpression scope expr of
                Right _ -> ok
                Left evalError -> err $ EEval evalError
        StWriteText _ -> ok
        StReturn -> ok
        StDeclaration (ident, type_) -> do
            scope <- get
            case HM.lookup ident scope of
                Nothing -> Right <$> modify (HM.insert ident (Left type_))
                Just _ -> err $ ERedeclararion ident
      where
        err = return . Left . one
        ok = return $ Right ()

findTexts :: [Statement] -> [Text]
findTexts = concatMap extractText
  where
    extractText = \case
        -- TODO: add labels to texts
        StWriteText text -> [text]
        StBlock stmts -> concatMap extractText stmts
        _ -> []

codegen :: Program -> Text
codegen (declarations, statements) =
    T.unlines
        . filter (/= T.empty)
        $ [ T.unlines $ zipWith declareStrConstant [1 ..] (findTexts statements)
          , "; External declaration of the printf function"
          , "declare i32 @printf(ptr noundef, ...)"
          , "@.int_printing = private unnamed_addr constant [3 x i8] c\"%d\\00\""
          , "@.double_printing = private unnamed_addr constant [3 x i8] c\"%f\\00\""
          , "@.str.s = private unnamed_addr constant [3 x i8] c\"%s\\00\""
          , "@.str.true = private unnamed_addr constant [5 x i8] c\"true\\00\""
          , "@.str.false = private unnamed_addr constant [6 x i8] c\"false\\00\""
          , "; Definition of main function"
          , "define i32 @main() {"
          , T.unlines
                . map indent
                . reverse
                . codeLines
                . execState (mapM_ genStatement statements)
                $ newCodeGen
          , indent "ret i32 0"
          , "}"
          ]
  where
    indentation = "    "
    indent = (indentation <>)
    genStatement :: Statement -> State CodeGen ()
    genStatement = \case
        StWriteText text -> do
            -- TODO: correct labels
            appendLine $ "call i32 @printf(ptr @str1) ; write " <> show text
            return undefined
        StWriteExpr expr -> do
            (value, typ) <- genExpr expr
            case typ of
                TypeInt ->
                    appendLine [i|call i32 (ptr, ...) @printf(ptr @.int_printing, i32 #{value})|]
                TypeDouble ->
                    appendLine [i|call i32 (ptr, ...) @printf(ptr @.double_printing, double #{value})|]
                TypeBool -> do
                    labelCast <- nextLabel
                    appendLine [i|#{labelCast} = trunc i8 #{value} to i1|]
                    -- NOTE: if there is some pointer error, this maybe useful
                    -- labelCast2 <- nextLabel
                    -- appendLine [i|#{labelCast2} = zext i8 #{value} to i64|]
                    labelSelect <- nextLabel
                    appendLine [i|#{labelSelect} = select i1 #{labelCast}, ptr @.str.true, ptr @.str.false|]
                    appendLine [i|call i32 (ptr, ...) @printf(ptr noundef @.str.s, ptr noundef #{labelSelect})|]

                    return ()
        StBlock _ -> undefined
        StExpression _ -> undefined
        StIf _ _ -> undefined
        StIfElse _ _ _ -> undefined
        StWhile _ _ -> undefined
        StRead _ -> undefined
        StReturn -> undefined
        StDeclaration _ -> undefined
        StAssignment _ _ -> undefined

    genExpr :: Expression -> State CodeGen ValueLabel
    genExpr = \case
        UnaryMinus _ -> undefined
        BitwiseNeg _ -> undefined
        LogicalNeg _ -> undefined
        IntCast _ -> undefined
        DoubleCast _ -> undefined
        BitwiseSum _ _ -> undefined
        BitwiseMult _ _ -> undefined
        Multiplication _ _ -> undefined
        Division _ _ -> undefined
        Addition left right -> do
            (labelLeft, typeLeft) <- genExpr left
            (labelRight, typeRight) <- genExpr right
            value <- nextLabel
            case (typeLeft, typeRight) of
                (TypeInt, TypeInt) -> do
                    appendLine [i|#{value} = add i32 #{labelLeft}, #{labelRight}|]
                    return (value, TypeInt)
        Subtraction _ _ -> undefined
        GreaterThen _ _ -> undefined
        GreaterThenEq _ _ -> undefined
        LessThen _ _ -> undefined
        LessThenEq _ _ -> undefined
        Equal _ _ -> undefined
        NotEqual _ _ -> undefined
        LogicOr _ _ -> undefined
        LogicAnd _ _ -> undefined
        Identifier _ -> undefined
        IntLiteral n -> do
            labelPtr <- nextLabel
            appendLine [i|#{labelPtr} = alloca i32|]
            appendLine [i|store i32 #{n}, ptr #{labelPtr}|]
            labelValue <- nextLabel
            appendLine [i|#{labelValue} = load i32, ptr #{labelPtr}|]
            return (labelValue, TypeInt)
        DoubleLiteral d -> do
            labelPtr <- nextLabel
            appendLine [i|#{labelPtr} = alloca double|]
            appendLine [i|store double #{d}, ptr #{labelPtr}|]
            labelValue <- nextLabel
            appendLine [i|#{labelValue} = load double, ptr #{labelPtr}|]
            return (labelValue, TypeDouble)
        BoolLiteral b -> do
            -- TODO: don't allocate memory for constants/literals
            labelPtr <- nextLabel
            appendLine [i|#{labelPtr} = alloca i8|]
            let bit :: Text = if b then "1" else "0"
            appendLine [i|store i8 #{bit}, ptr #{labelPtr}|]
            labelValue <- nextLabel
            appendLine [i|#{labelValue} = load i8, ptr #{labelPtr}|]
            return (labelValue, TypeBool)

-- store i32 %5, ptr %2, align 4
--

-- %1 = alloca i32, align 4
-- %2 = alloca i32, align 4
-- store i32 0, ptr %1, align 4
-- store i32 3, ptr %2, align 4
-- %3 = load i32, ptr %2, align 4

-- genExpr :: Expression -> State CodeGen ValueLabel
-- genExpr = \case
--     Addition (IntLiteral left) (IntLiteral right) ->
--         ([[i|%_ret = add i32 #{left}, #{right} ; #{left} + #{right}|]], "%_ret", TypeInt)
--     IntLiteral n -> (one [i|call i32 @printf(ptr @.int_printing, i32 #{n}) ; write #{n}|], undefined, undefined)
--     DoubleLiteral d -> ([[i|call i32 (ptr, ...) @printf(ptr @.double_printing, double #{d}) ; write #{d}|]], undefined, undefined)
--     BoolLiteral b -> ([[i|call i32 @printf(ptr @#{pickLiteral b}) ; write #{b}|]], undefined, undefined)
--       where
--         pickLiteral :: Bool -> Text
--         pickLiteral bool = if b then ".true" else ".false"
appendLines :: [Text] -> State CodeGen ()
appendLines = mapM_ appendLine

appendLine :: Text -> State CodeGen ()
appendLine line = do
    CodeGen{codeLines} <- get
    modify (\c -> c{codeLines = line : codeLines})
    return ()

nextLabel :: State CodeGen Text
nextLabel = do
    CodeGen{counter} <- get
    modify (\c -> c{counter = counter + 1})
    return [i|%#{counter}|]

type ValueLabel = (Text, VarType)

data CodeGen = CodeGen
    { codeLines :: ![Text] -- NOTE: the lines are reversed for performance
    , counter :: Int
    , indentationLevel :: Int
    }
newCodeGen :: CodeGen
newCodeGen =
    CodeGen
        { codeLines = []
        , counter = 1
        , indentationLevel = 0
        }

declareStrConstant :: Int -> Text -> Text
declareStrConstant n text =
    [i|@str#{n} = private unnamed_addr constant [#{len} x i8] c"#{escaped}"|]
  where
    len = T.length text + 1
    escaped = T.replace "\n" "\\0A" text <> "\\00"
