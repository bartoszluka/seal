{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seal.Compiler (typecheck, Error (..), codegen) where

import Data.HashMap.Strict qualified as HM
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
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

codegen :: Program -> Text
codegen (declarations, statements) =
    let
        CodeGen{codeLines, textConstants} =
            flip execState newCodeGen $
                mapM_ genStatement $
                    map StDeclaration declarations <> statements
     in
        T.unlines
            . filter (/= T.empty)
            $ [ "; External declaration of the printf function"
              , "declare i32 @printf(ptr noundef, ...)"
              , "; constants for printfing values"
              , "@.int_printing = private unnamed_addr constant [3 x i8] c\"%d\\00\""
              , "@.double_printing = private unnamed_addr constant [3 x i8] c\"%g\\00\""
              , "@.str.s = private unnamed_addr constant [3 x i8] c\"%s\\00\""
              , "@.str.true = private unnamed_addr constant [5 x i8] c\"true\\00\""
              , "@.str.false = private unnamed_addr constant [6 x i8] c\"false\\00\""
              , if Seq.null textConstants then "" else "; constants for texts"
              , T.unlines . toList $ textConstants
              , "; Definition of main function"
              , "define i32 @main() {"
              , T.unlines
                    . toList
                    . fmap indent
                    $ codeLines
              , indent [i|br label %#{endMainLabel}|]
              , indent [i|#{endMainLabel}:|]
              , indent "ret i32 0"
              , "}"
              ]
  where
    indentation = "    "
    indent = (indentation <>)
    genStatement :: Statement -> State CodeGenState ()
    genStatement = \case
        StWriteText text -> do
            label <- declareTextConstant text
            value <- nextTemporaryVariable
            appendLine [i|#{value} = call i32 @printf(ptr #{label})|]
            return ()
        StWriteExpr expr -> do
            (value, typ) <- genExpr expr
            case typ of
                TypeInt ->
                    appendLine [i|call i32 (ptr, ...) @printf(ptr @.int_printing, i32 #{value})|]
                TypeDouble ->
                    appendLine [i|call i32 (ptr, ...) @printf(ptr @.double_printing, double #{value})|]
                TypeBool -> do
                    labelCast <- nextTemporaryVariable
                    appendLine [i|#{labelCast} = trunc i8 #{value} to i1|]
                    -- NOTE: if there is some pointer error, this maybe useful
                    -- labelCast2 <- nextVariable
                    -- appendLine [i|#{labelCast2} = zext i8 #{value} to i64|]
                    labelSelect <- nextTemporaryVariable
                    appendLine [i|#{labelSelect} = select i1 #{labelCast}, ptr @.str.true, ptr @.str.false|]

                    value <- nextTemporaryVariable
                    appendLine [i|#{value} = call i32 (ptr, ...) @printf(ptr noundef @.str.s, ptr noundef #{labelSelect})|]

                    return ()
        StBlock stmts ->
            mapM_ genStatement stmts
        StExpression expr ->
            genExpr expr $> ()
        StIf condition statement -> do
            -- TODO: typecheck?
            (conditionVar, typ) <- genExpr condition
            ifTrue <- nextLabel
            endIf <- nextLabel
            castToBool <- nextTemporaryVariable
            appendLine [i|#{castToBool} = trunc i8 #{conditionVar} to i1|]
            appendLine [i|br i1 #{castToBool}, label %#{ifTrue}, label %#{endIf}|]
            appendLine [i|#{ifTrue}:|]
            genStatement statement
            appendLine [i|br label %#{endIf}|]
            appendLine [i|#{endIf}:|]
        StIfElse condition onTrue onFalse -> do
            -- TODO: typecheck?
            (conditionVar, typ) <- genExpr condition
            ifTrue <- nextLabel
            ifFalse <- nextLabel
            endIfElse <- nextLabel
            castToBool <- nextTemporaryVariable
            appendLine [i|#{castToBool} = trunc i8 #{conditionVar} to i1|]
            appendLine [i|br i1 #{castToBool}, label %#{ifTrue}, label %#{ifFalse}|]
            appendLine [i|#{ifTrue}:|]
            genStatement onTrue
            appendLine [i|br label %#{endIfElse}|]
            appendLine [i|#{ifFalse}:|]
            genStatement onFalse
            appendLine [i|br label %#{endIfElse}|]
            appendLine [i|#{endIfElse}:|]
        StWhile condition statement -> do
            -- TODO: typecheck?
            startWhile <- nextLabel
            appendLine [i|br label %#{startWhile}|]
            appendLine [i|#{startWhile}:|]
            (conditionVar, typ) <- genExpr condition
            ifTrue <- nextLabel
            endWhile <- nextLabel
            castToBool <- nextTemporaryVariable
            appendLine [i|#{castToBool} = trunc i8 #{conditionVar} to i1|]

            appendLine [i|br i1 #{castToBool}, label %#{ifTrue}, label %#{endWhile}|]
            appendLine [i|#{ifTrue}:|]
            genStatement statement

            appendLine [i|br label %#{startWhile}|]
            appendLine [i|#{endWhile}:|]
        StRead _ -> undefined
        StReturn ->
            appendLine [i|br label #{endMainLabel}|]
        StDeclaration (name, typ) -> do
            let var = variableName name
            declareVariable name typ
            appendLine [i|#{var} = alloca #{getLlvmType typ}|]
        StAssignment name expr -> do
            (value, typ) <- genExpr expr
            let var = variableName name
            result <- assignVariable name
            case result of
                Right () -> appendLine [i|store #{getLlvmType typ} #{value}, ptr #{var}|]
                --  TODO: errors
                Left notFound -> undefined

    genExpr :: Expression -> State CodeGenState ValueLabel
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
            case (typeLeft, typeRight) of
                (TypeInt, TypeInt) -> do
                    value <- nextTemporaryVariable
                    appendLine [i|#{value} = add i32 #{labelLeft}, #{labelRight}|]
                    return (value, TypeInt)
                (TypeDouble, TypeInt) ->
                    castAndAdd labelLeft labelRight
                (TypeInt, TypeDouble) ->
                    castAndAdd labelRight labelLeft
                _ ->
                    -- TODO: type errors
                    return undefined
          where
            castAndAdd :: Text -> Text -> State CodeGenState (Text, VarType)
            castAndAdd labelDouble labelInt = do
                castLabel <- nextTemporaryVariable
                appendLine [i|#{castLabel} = sitofp i32 #{labelInt} to double|]
                label <- nextTemporaryVariable
                appendLine [i|#{label} = fadd double #{castLabel}, #{labelDouble}|]
                return (label, TypeDouble)
        Subtraction _ _ -> undefined
        GreaterThen _ _ -> undefined
        GreaterThenEq _ _ -> undefined
        LessThen left right -> do
            (valL, typeL) <- genExpr left
            (valR, typeR) <- genExpr right
            --  TODO: typechecking

            result<- nextTemporaryVariable
            appendLine [i|#{result} = icmp slt i32 #{valL}, #{valR}|]
            return (result, TypeBool)
        LessThenEq _ _ -> undefined
        Equal left right -> do
            (valL, typeL) <- genExpr left
            (valR, typeR) <- genExpr right
            --  TODO: typechecking

            result<- nextTemporaryVariable
            appendLine [i|#{result} = icmp eq i32 #{valL}, #{valR}|]
            return (result, TypeBool)
        NotEqual left right -> do
            (valL, typeL) <- genExpr left
            (valR, typeR) <- genExpr right
            --  TODO: typechecking

            result<- nextTemporaryVariable
            appendLine [i|#{result} = icmp ne i32 #{valL}, #{valR}|]
            return (result, TypeBool)

        LogicOr _ _ -> undefined
        LogicAnd _ _ -> undefined
        Identifier name -> do
            mbType <- getVariable name
            case mbType of
                --  TODO: errors
                Nothing -> undefined
                Just typ -> do
                    labelValue <- nextTemporaryVariable
                    appendLine [i|#{labelValue} = load #{getLlvmType typ}, ptr #{variableName name}|]
                    return (labelValue, typ)
        IntLiteral n -> do
            labelPtr <- nextTemporaryVariable
            appendLine [i|#{labelPtr} = alloca i32|]
            appendLine [i|store i32 #{n}, ptr #{labelPtr}|]
            labelValue <- nextTemporaryVariable
            appendLine [i|#{labelValue} = load i32, ptr #{labelPtr}|]
            return (labelValue, TypeInt)
        DoubleLiteral d -> do
            labelPtr <- nextTemporaryVariable
            appendLine [i|#{labelPtr} = alloca double|]
            appendLine [i|store double #{d}, ptr #{labelPtr}|]
            labelValue <- nextTemporaryVariable
            appendLine [i|#{labelValue} = load double, ptr #{labelPtr}|]
            return (labelValue, TypeDouble)
        BoolLiteral b -> do
            -- TODO: don't allocate memory for constants/literals
            labelPtr <- nextTemporaryVariable
            appendLine [i|#{labelPtr} = alloca i8|]
            let bit :: Text = if b then "1" else "0"
            appendLine [i|store i8 #{bit}, ptr #{labelPtr}|]
            labelValue <- nextTemporaryVariable
            appendLine [i|#{labelValue} = load i8, ptr #{labelPtr}|]
            return (labelValue, TypeBool)

getLlvmType :: VarType -> Text
getLlvmType = \case
    TypeInt -> "i32"
    TypeBool -> "i8"
    TypeDouble -> "double"

assignVariable :: Identifier -> State CodeGenState (Either Identifier ())
assignVariable name = do
    CodeGen{variables} <- get
    case HM.lookup name variables of
        Just typ -> do
            modify (\c -> c{variables = HM.insert name typ variables})
            return $ Right ()
        Nothing -> return $ Left name

declareVariable :: Identifier -> VarType -> State CodeGenState ()
declareVariable name typ = do
    CodeGen{variables} <- get
    modify (\c -> c{variables = HM.insert name (Left typ) variables})

endMainLabel :: Text
endMainLabel = "END_MAIN"

appendLines :: [Text] -> State CodeGenState ()
appendLines = mapM_ appendLine

appendLine :: Text -> State CodeGenState ()
appendLine line = do
    CodeGen{codeLines} <- get
    modify (\c -> c{codeLines = codeLines :|> line})
    return ()

declareTextConstant :: Text -> State CodeGenState Text
declareTextConstant text = do
    cg@CodeGen{textConstants} <- get
    let n = Seq.length textConstants + 1
        (constant, label) = buildConstant n text
    put $
        cg
            { textConstants = textConstants :|> constant
            }
    return label

buildConstant :: Int -> Text -> (Text, Text)
buildConstant n text = (constant, label)
  where
    constant = [i|#{label} = private unnamed_addr constant [#{T.length text + 1} x i8] c"#{escaped}"|]
    label = [i|@.str.#{n}|] :: Text
    escaped = T.replace "\n" "\\0A" text <> "\\00"

nextLabel :: State CodeGenState Text
nextLabel = do
    CodeGen{labelCounter} <- get
    modify (\c -> c{labelCounter = labelCounter + 1})
    return [i|jump_label_#{labelCounter}|]

nextTemporaryVariable :: State CodeGenState Text
nextTemporaryVariable = do
    CodeGen{statementCounter} <- get
    modify (\c -> c{statementCounter = statementCounter + 1})
    return [i|%#{statementCounter}|]

variableName :: Text -> Text
variableName name = [i|%.uservar.#{name}|]

getVariable :: Text -> State CodeGenState (Maybe VarType)
getVariable name = do
    CodeGen{variables} <- get
    return $ either id id <$> HM.lookup name variables

type ValueLabel = (Text, VarType)

data CodeGenState = CodeGen
    { codeLines :: Seq Text
    , statementCounter :: Int
    , labelCounter :: Int
    , indentationLevel :: Int
    , textConstants :: Seq Text
    , variables :: HashMap Identifier (Either VarType VarType)
    }

newCodeGen :: CodeGenState
newCodeGen =
    CodeGen
        { codeLines = Seq.empty
        , statementCounter = 1
        , labelCounter = 1
        , indentationLevel = 0
        , textConstants = Seq.empty
        , variables = HM.empty
        }
