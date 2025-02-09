{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid partial function" #-}
module TypeChecker.Checker where

import AST
import Algebra.Lattice ((\/))
import Data.Either.Extra
import Data.Function (on)
import Data.List (find, findIndex)
import Data.Map.Strict (Map, insert, member, union, (!), (!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import TypeChecker.ConstExprSolver (solveConstExpr)
import TypeChecker.TypeUtils
import Utils
import Prelude hiding (error, id)

f :: Block ConstantSolverOutput -> Block TypeCheckerOutput
f tree =
    let t1 = (fmap . fmap) inToStep1 tree
        t2 = t1 -- solveConstExpr t1
        t3 = (fmap . fmap) stepnToOut t2
    in  t3

data SymType = Variable | Function
    deriving (Eq, Ord, Show)

type SymEntry = (Int, Instruction Step)

type SymStack = [Map SymType (Map Ident SymEntry)]

data Status = Ok | Fail
    deriving (Eq, Show)

rStep3 = fillOutStep3 RightValue

identRStep3 tpe binding = pass1 updateAnn (rStep3 tpe (Just binding) . ann)

eRStep3 tpe = pass1 updateAnn (rStep3 tpe Nothing . ann)

addBind :: SymStack -> Instruction Step -> Either SymEntry SymStack
addBind symStack decl
    | (VariableDecl _ id _ _ Step2{}) <- decl = f id Variable
    | (FunctionDecl _ id _ _ _ _) <- decl = f id Function
    | otherwise = "instruction" `unexpectedIn` "addBind"
    where
        f id symType = case findIndex (\t -> id `member` (t Map.! symType)) symStack of
            Just i
                | i == 0 ->
                    let
                        symTable = symStack !! i
                        binding = (symTable ! symType) ! id
                    in
                        Left binding
            _ ->
                let
                    depth = length symStack - 1
                    (symTable1 : symTables) = symStack
                    symTable2 = symTable1 ! symType
                in
                    Right $ insert symType (insert id (depth, decl) symTable2) symTable1 : symTables

queryBind :: SymStack -> Expr Step -> Maybe SymEntry
queryBind symStack decl
    | (Id _ id Step2{}) <- decl = f id Variable
    | (FunctionCall _ id _ _) <- decl = f id Function
    | otherwise = "expression" `unexpectedIn` "queryBind"
    where
        f id symType = case find (\t -> id `member` (t ! symType)) symStack of
            Just symTable1 -> Just $ (symTable1 ! symType) ! id
            Nothing -> Nothing

buildDeclType :: DeclType Step -> Type
buildDeclType declType = case solveVarDeclType declType ErrorType of
    (Fail, ErrorType) -> ErrorType
    (Fail, tpe) -> tpe
    _ -> "status" `unexpectedDuring` "buildDeclType"

solveVarDeclType :: DeclType Step -> Type -> (Status, Type)
solveVarDeclType DBoolType initType
    | BoolType == initType = (Ok, BoolType)
    | ErrorType == initType = (Fail, BoolType)
solveVarDeclType DCharType initType
    | CharType == initType = (Ok, CharType)
    | ErrorType == initType = (Fail, CharType)
solveVarDeclType DIntType initType
    | IntType == initType = (Ok, IntType)
    | ErrorType == initType = (Fail, IntType)
solveVarDeclType DStringType initType
    | StringType == initType = (Ok, StringType)
    | ErrorType == initType = (Fail, StringType)
solveVarDeclType DFloatType initType
    | FloatType == initType = (Ok, FloatType)
    | ErrorType == initType = (Fail, FloatType)
solveVarDeclType (DPointerType declType) initType
    | PointerType tpe <- initType = PointerType <$> solveVarDeclType declType tpe
    | ErrorType == initType = PointerType <$> solveVarDeclType declType ErrorType
solveVarDeclType (DArrayType Nothing declType) initType
    | ArrayType len initType <- initType = ArrayType len <$> solveVarDeclType declType initType
    | otherwise = (Fail, ErrorType)
solveVarDeclType (DArrayType (Just (IntLiteral _ len1 Step2{sType = IntType})) declType) initType -- perhaps too rigid
    | (ArrayType len2 tpe) <- initType =
        if len1 == len2
            then
                ArrayType len1 <$> solveVarDeclType declType tpe
            else
                ArrayType len1 <$> solveVarDeclType declType ErrorType
    | ErrorType == initType = ArrayType len1 <$> solveVarDeclType declType ErrorType
solveVarDeclType (DArrayType{}) _ = (Fail, ErrorType)
solveVarDeclType declType _ = solveVarDeclType declType ErrorType

promote expr1 expr2 = (promote expr1, promote expr2)
    where
        sup = eType expr1 \/ eType expr2
        promote expr
            | eType expr /= sup = UnaryOp (position expr) Coercion expr $ newStep3 RightValue sup
            | otherwise = expr

-- checkTypes = emapAccumBlock f g [Map.empy]

checkExpr :: SymStack -> Expr Step -> Expr Step
checkExpr symStack = emap f
    where
        f :: Expr Step -> Expr Step
        f ident@(Id{}) = case queryBind symStack ident of
            Just binding@(_, decl) -> identRStep3 (eType decl) binding ident
            Nothing -> UnknownSymbol |< ident
        f expr@(UnaryOp _ op subExpr _) =
            case satisfiesUnOp op subExpr of
                Just (got, expected) -> TypeMismatch got expected |< eRStep3 ErrorType expr
                Nothing -> eRStep3 (eType subExpr) expr
        f expr@(BinaryOp pos op subExpr1 subExpr2 x) =
            case satisfiesBinOp op subExpr1 subExpr2 of
                (Nothing, Nothing) ->
                    let (expr1, expr2) = promote subExpr1 subExpr2
                    in  BinaryOp pos op expr1 expr2 x
                (err1, err2) ->
                    let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |<)
                    in  f err1 . f err2 $ eRStep3 ErrorType expr
        f expr@(Ref _ subExpr _)
            | LeftValue <- eSide subExpr = eRStep3 (eType subExpr) expr
            | otherwise <- UnexpectedRightValue |< eRStep3 ErrorType expr
        f expr@(Deref pos subExpr x) = Deref pos expr x
        f (ArrayAcc pos expr1 expr2 x) = ArrayAcc pos expr1 expr2 x
        f (FunctionCall pos id exprs x) = FunctionCall pos id exprs x
        f (RangedArray pos expr1 expr2 x) = RangedArray pos expr1 expr2 x
        f expr = step2ToStep3 . assertGeqStep 2 <$> expr -- literals
        -- (IntLiteral pos v x) -> IntLiteral pos v x
        -- (CharLiteral pos v x) -> CharLiteral pos v x
        -- (StringLiteral pos v x) -> StringLiteral pos v x
        -- (FloatLiteral pos v x) -> FloatLiteral pos v x
        -- (BoolLiteral pos v x) -> BoolLiteral pos v x
        -- (ArrayLiteral pos exprs x) -> ArrayLiteral pos exprs x

desInstruction :: SymStack -> Instruction Step -> (SymStack, Instruction Step)
desInstruction symTable (VariableDecl pos id declType expr Step2{sType = tpe, sSide = side}) = 0
    where
        tpe = 0

g = 0
