{-# LANGUAGE QuasiQuotes #-}

module Seal.Compiler (typecheck, Error (..), codegen) where

import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate (i, iii)
import Data.Text qualified as T
import Relude
import Seal.Interpreter (EvalError, Value (..), evalExpression)
import Seal.Parser (Declaration, Identifier, Program, Statement (..), VarType (..))

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
        [ T.intercalate "\n" $ zipWith declareStrConstant [1 ..] (findTexts statements)
        , "; External declaration of the puts function"
        , "declare i32 @puts(ptr nocapture) nounwind"
        , "; Definition of main function"
        , "define i32 @main() {"
        , T.intercalate "\n" (pad . genStatement <$> statements)
        , pad "ret i32 0"
        , "}"
        ]
  where
    padding = "    "
    pad = (padding <>)
    genStatement :: Statement -> Text
    genStatement = \case
        StWriteText text -> "call i32 @puts(ptr @str1) ; write " <> show text

declareStrConstant :: Int -> Text -> Text
declareStrConstant n text =
    [i|@str #{n} = private unnamed_addr constant [#{len} x i8 "#{escaped}"|]
  where
    len = T.length text + 1
    escaped = T.replace "\n" "\\0A" text <> "\\00"
