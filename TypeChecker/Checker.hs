{-# HLINT ignore "Avoid partial function" #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TypeChecker.Checker where

import AST
import Algebra.Lattice (Lattice ((/\)), joinLeq, (\/))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either.Extra
import Data.List (find, findIndex)
import Data.Map.Strict (Map, insert, member, (!))
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumL)
import Debug.Trace (trace)
import TypeChecker.TypeUtils
import Utils
import Prelude hiding (error, id)

data SymType = Variable | Function
    deriving (Eq, Ord, Show)

type SymEntry = (Int, Modality, Instruction Step)

type SymStack = [Map SymType (Map Ident SymEntry)]

data Status = Status
    { symStack :: SymStack,
      function :: Maybe Type,
      loop :: Maybe Type
    }

-- todo
-- array literal elimination with codegen
-- graph cycle detection for function initializations/function captures

-- requests:
-- every var symbol annotated with ref/value
-- every accessor must give lvalue, array valueness tracking must be done covertly
-- resolve deref pairs on leftSides statically

rStep3 = flip (fillOutStep3 RightValue) Nothing

lStep3 = flip (fillOutStep3 LeftValue) Nothing

rBindStep3 = fillOutStep3 RightValue

eLBindStep3 tpe binding = pass1 updateAnn (fillOutStep3 LeftValue tpe (Just binding) . ann)

eRStep3 tpe = pass1 updateAnn (rStep3 tpe . ann)

eLStep3 tpe = pass1 updateAnn (lStep3 tpe . ann)

we |?< e
    | ErrorType <- eType e = e
    | otherwise = we |< e

addBind :: SymStack -> Modality -> Instruction Step -> Either SymEntry SymStack
addBind symStack modality decl
    | (VariableDecl _ id _ _ _) <- decl = f id modality Variable
    | (FunctionDecl _ id _ _ _ _) <- decl = f id ModalityRef Function
    | otherwise = "instruction" `unexpectedIn` "addBind"
    where
        f id modality symType = case findIndex (\t -> id `member` (t Map.! symType)) symStack of
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
                    Right $ insert symType (insert id (depth, modality, decl) symTable2) symTable1 : symTables

queryBind :: SymStack -> Expr Step -> Maybe SymEntry
queryBind symStack decl
    | (Id _ id Step2{}) <- decl = f id Variable
    | (FunctionCall _ id _ _) <- decl = f id Function
    | otherwise = "expression" `unexpectedIn` "queryBind"
    where
        f id symType = case find (\t -> id `member` (t ! symType)) symStack of
            Just symTable1 -> Just $ (symTable1 ! symType) ! id
            Nothing -> Nothing

pushEnv :: SymStack -> SymStack
pushEnv symStack = Map.fromList [(Variable, Map.empty), (Function, Map.empty)] : symStack

buildDeclType :: DeclType Step -> Type
buildDeclType declType = case solveVarDeclType declType ErrorType of
    (False, ErrorType) -> ErrorType
    (False, tpe) -> tpe
    _ -> "status" `unexpectedDuring` "buildDeclType"

solveVarDeclType :: DeclType Step -> Type -> (Bool, Type)
solveVarDeclType DBoolType initType
    | BoolType == initType = (True, BoolType)
    | ErrorType == initType = (False, BoolType)
solveVarDeclType DCharType initType
    | CharType == initType = (True, CharType)
    | ErrorType == initType = (False, CharType)
solveVarDeclType DIntType initType
    | IntType == initType = (True, IntType)
    | ErrorType == initType = (False, IntType)
solveVarDeclType DStringType initType
    | StringType == initType = (True, StringType)
    | ErrorType == initType = (False, StringType)
solveVarDeclType DFloatType initType
    | FloatType == initType = (True, FloatType)
    | ErrorType == initType = (False, FloatType)
solveVarDeclType (DPointerType declType) initType
    | PointerType tpe <- initType = PointerType <$> solveVarDeclType declType tpe
    | ErrorType == initType = PointerType <$> solveVarDeclType declType ErrorType
solveVarDeclType (DArrayType Nothing declType) initType
    | ArrayType len initType <- initType = ArrayType len <$> solveVarDeclType declType initType
    | otherwise = (False, ErrorType)
solveVarDeclType (DArrayType (Just (IntLiteral _ len1 Step2{sType = IntType})) declType) initType -- perhaps too rigid
    | len1 < 1 = (False, ErrorType) -- TODO, better error?
    | (ArrayType len2 tpe) <- initType =
        if len1 == len2
            then
                ArrayType len1 <$> solveVarDeclType declType tpe
            else
                ArrayType len1 <$> solveVarDeclType declType ErrorType
    | ErrorType == initType = ArrayType len1 <$> solveVarDeclType declType ErrorType
solveVarDeclType (DArrayType{}) _ = (False, ErrorType)
solveVarDeclType declType _ = solveVarDeclType declType ErrorType

functionDeclPass (FunctionDecl pos id args declType block x) =
    case FunctionDecl pos id newArgDeclTypes declType block (rStep3 (FunctionType argTypes tpe) x) of
        t | ErrorType <- returnType -> UnsolvableType |< t
        t -> t
    where
        tpe =
            if ErrorType `notElem` (snd <$> argTypes)
                then
                    returnType
                else
                    ErrorType
        returnType = buildDeclType declType
        newArgDeclTypes =
            [ let newArg = eRStep3 tpe arg
              in  if ErrorType == tpe
                    then UnsolvableType |< newArg
                    else newArg
              | (_, tpe) <- argTypes
              | arg <- args
            ]
        argTypes = [(modty, buildDeclType declType) | (Param modty _ declType _) <- args]
functionDeclPass is = is

buildArrayLiteral (ArrayLiteral pos exprs x) = ArrayLiteral pos (promoteList (sup <$ exprTypes) exprs) newX
    where
        newX = rStep3 (ArrayType (toInteger $ length exprs) sup) x
        exprTypes = eType <$> exprs
        sup = foldl1 (/\) exprTypes
buildArrayLiteral _ = "expression" `unexpectedIn` "buildArrayLiteral"

promote expr1 expr2 = (f expr1, f expr2)
    where
        sup = eType expr1 \/ eType expr2
        f expr
            | eType expr /= sup = UnaryOp (position expr) Coercion expr $ newStep3 RightValue sup
            | otherwise = expr

promoteList = zipWith f
    where
        f tpe expr
            | eType expr `joinLeq` tpe = UnaryOp (position expr) Coercion expr $ newStep3 RightValue tpe
            | otherwise = expr

checkTree = checkBlock (Status (pushEnv []) Nothing Nothing)

checkExpr :: SymStack -> Expr Step -> Expr Step
checkExpr symStack = emap f
    where
        f :: Expr Step -> Expr Step
        f expr
            | Step2{} <- ann expr = step2ToStep3 . assertGeqStep 2 <$> expr -- literals
        f ident@(Id{}) = case queryBind symStack ident of
            Just binding@(_, _, decl) -> eLBindStep3 (eType decl) binding ident
            Nothing -> UnknownSymbol |< eLStep3 ErrorType ident
        f expr@(UnaryOp pos op subExpr x) =
            case satisfiesUnOp op subExpr of
                Nothing -> eRStep3 (eType subExpr) expr
                Just (got, expected) -> UnaryOp pos op (TypeMismatch got expected |?< subExpr) (rStep3 ErrorType x)
        f (BinaryOp pos op subExpr1 subExpr2 x) =
            case satisfiesBinOp op subExpr1 subExpr2 of
                (Nothing, Nothing) ->
                    let sup = eType subExpr1 \/ eType subExpr2
                        (newSubExpr1, newSubExpr2) = promote subExpr1 subExpr2
                    in  BinaryOp pos op newSubExpr1 newSubExpr2 (rStep3 sup x)
                (err1, err2) ->
                    let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                    in  BinaryOp pos op (f err1 subExpr1) (f err2 subExpr2) (rStep3 ErrorType x)
        f expr@(Ref pos subExpr x) =
            case satisfiesRef subExpr of
                Nothing -> ePushPointer (eType subExpr) expr
                Just (got, expected) -> Ref pos (TypeMismatch got expected |?< subExpr) (rStep3 ErrorType x)
        f expr@(Deref pos subExpr x) =
            case satisfiesDeref subExpr of
                Nothing -> ePopPointer (eType subExpr) expr
                Just (got, expected) -> Deref pos (TypeMismatch got expected |?< subExpr) (lStep3 ErrorType x)
        f expr@(ArrayAcc pos indExpr subExpr x) =
            case satisfiesAccessor indExpr subExpr of
                (Nothing, Nothing) -> ePopArray (eType subExpr) expr
                (err1, err2) ->
                    let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                    in  ArrayAcc pos (f err1 indExpr) (f err2 subExpr) (rStep3 ErrorType x)
        f fcall@(FunctionCall pos id subExprs x) =
            case queryBind symStack fcall of
                Just binding@(_, _, decl) ->
                    case satisfiesFCall fType subExprs of
                        (False, argErrors)
                            | Nothing <- foldl (<|>) Nothing argErrors ->
                                FunctionCall pos id (promoteList (snd <$> argTypes) subExprs) (rBindStep3 retType (Just binding) x)
                        (argCount, argErrors) ->
                            let newX = if argCount then ArgCount |< x else x
                            in  FunctionCall pos id (f <$> argErrors <*> subExprs) (rBindStep3 ErrorType (Just binding) newX)
                    where
                        fType = eType decl
                        (FunctionType argTypes retType) = fType
                        f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                Nothing -> UnknownSymbol |< eRStep3 ErrorType fcall
        f expr@(ArrayLiteral{}) =
            case satisfiesArrayLiteral expr of
                (False, False) -> buildArrayLiteral expr
                (True, _) -> EmptyArray |?< eRStep3 ErrorType expr
                _ -> eRStep3 ErrorType expr
        f (RangedArray pos expr1 expr2 x) = RangedArray pos expr1 expr2 x
        f expr = ("expression annotation (" ++ show (ann expr) ++ ")") `unexpectedDuring` "checkExpr"

-- (IntLiteral pos v x) -> IntLiteral pos v x
-- (CharLiteral pos v x) -> CharLiteral pos v x
-- (StringLiteral pos v x) -> StringLiteral pos v x
-- (FloatLiteral pos v x) -> FloatLiteral pos v x
-- (BoolLiteral pos v x) -> BoolLiteral pos v x

checkBlock status@Status{symStack} block = emapAccumLBlock checkInstruction status{symStack = newSymStack} block2
    where
        (newSymStack, block2) = mapAccumL f (pushEnv symStack) block1
        block1 = emapBlock functionDeclPass block
        f symStack fdecl@FunctionDecl{}
            | ErrorType <- eType fdecl = (symStack, fdecl)
            | otherwise = case addBind symStack ModalityRef fdecl of
                Left (_, _, decl) -> (symStack, VariableAlreadyDefined (void decl) |< fdecl)
                Right symStack -> (symStack, fdecl)
        f symStack is = (symStack, is)

checkInstruction :: Status -> Instruction Step -> (Status, Instruction Step)
checkInstruction status@Status{symStack} (VariableDecl pos id declType expr x) =
    if ok -- no, todo
        then case addBind symStack ModalityVal variableDecl of
            (Left (_, _, oldDecl)) -> (status, VariableAlreadyDefined (void oldDecl) |< variableDecl)
            (Right symStack) -> (status{symStack}, variableDecl)
        else case tpe of
            ErrorType -> (status, UnsolvableType |< variableDecl)
            _ -> (status, variableDecl)
    where
        variableDecl = VariableDecl pos id declType newExpr (rStep3 tpe x)
        exprType = eType newExpr
        (ok, tpe) = solveVarDeclType declType exprType
        newExpr = case emap (checkExpr symStack) expr of
            t | not ok && ErrorType /= tpe -> TypeMismatch exprType (Left tpe) |?< t
            t -> t
checkInstruction status@Status{symStack} (FunctionDecl pos id args declType block x@Step3{sType = FunctionType argTypes retType}) =
    (status, FunctionDecl pos id newArgs declType newBlock x)
    where
        newBlock = checkBlock status{symStack = newSymStack, function = Just retType, loop = Nothing} block
        (newSymStack, newArgs) = mapAccumL f (pushEnv symStack) $ zip bindings args
        bindings =
            -- this was tragic mistake
            [ (modty, VariableDecl pos id declType (Id pos id x) x)
              | ((modty, _), Param _ id declType x) <-
                    zip argTypes args
            ]
        f symStack (binding@(_, decl), arg)
            | ErrorType <- eType decl = (symStack, arg)
            | otherwise = case uncurry (addBind symStack) binding of
                Left (_, _, decl) -> (symStack, VariableAlreadyDefined (void decl) |< arg)
                Right symStack -> (symStack, arg)
checkInstruction status@Status{symStack} e@(Assignment pos expr1 op expr2 x) = (status, e) -- TODO
checkInstruction status@Status{symStack} (NestedBlock pos block x) =
    (status, NestedBlock pos newBlock (rStep3 VoidType x))
    where
        newBlock = checkBlock status{symStack = pushEnv symStack} block
checkInstruction status@Status{symStack} (While pos expr block x) =
    (status, While pos newExpr newBlock (rStep3 VoidType x))
    where
        newExpr = case checkExpr symStack expr of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
        newBlock = checkBlock status{symStack = pushEnv symStack} block
checkInstruction status@Status{symStack} (IfThen pos expr block x) =
    (status, IfThen pos newExpr newBlock (rStep3 VoidType x))
    where
        newExpr = case checkExpr symStack expr of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
        newBlock = checkBlock status{symStack = pushEnv symStack} block
checkInstruction status@Status{symStack} (IfThenElse pos expr block1 block2 x) =
    (status, IfThenElse pos newExpr newBlock1 newBlock2 (rStep3 VoidType x))
    where
        newExpr = case checkExpr symStack expr of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
        newBlock1 = checkBlock status{symStack = pushEnv symStack} block1
        newBlock2 = checkBlock status{symStack = pushEnv symStack} block2
checkInstruction status@Status{symStack} (Expression pos expr x) =
    (status, Expression pos newExpr (rStep3 VoidType x))
    where
        newExpr = checkExpr symStack expr
checkInstruction status e = (status, e) -- TODO -- TODO

-- Param Modality Ident (DeclType a) a
