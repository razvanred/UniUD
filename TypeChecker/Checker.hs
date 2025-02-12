{-# HLINT ignore "Avoid partial function" #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
      loop :: Bool
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
    | (VariableDecl _ id _ _ _) <- decl = trace "foo32" $ f id modality Variable
    | (FunctionDecl _ id _ _ _ _) <- decl = trace "foo21w" $ (trace "foo2f3" $ f) id ModalityRef Function
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
buildDeclType declType = case solveVarDeclType (trace "pippus" declType) (trace "ppp" ErrorType) of
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
solveVarDeclType DVoidType initType
    | VoidType == initType = (True, VoidType)
    | ErrorType == initType = (False, VoidType)
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
solveVarDeclType declType _ = solveVarDeclType (trace "a" declType) ErrorType

functionDeclPass symStack (FunctionDecl pos id args' declType block x) =
    case FunctionDecl pos id args declType block (rStep3 (FunctionType argTypes tpe) x) of
        fDecl | ErrorType <- returnType -> (symStack, UnsolvableType |< fDecl)
        fDecl | ErrorType <- tpe -> (symStack, fDecl)
        fDecl -> case trace "foo76" $ addBind symStack ModalityRef fDecl of
            Left (_, _, oldFDecl) -> (symStack, FunctionAlreadyDefined (void oldFDecl) |< fDecl)
            Right symStack -> (symStack, fDecl)
    where
        returnType = trace "foo123" $ buildDeclType declType
        argTypes = trace "foo1sdgew" $ [(modty, buildDeclType declType) | (Param modty _ declType _) <- args']
        tpe
            | ErrorType `notElem` (snd <$> argTypes) = trace "foo11sdfv1" $ returnType
            | otherwise = trace "foo235f1" $ ErrorType
        args =
            [ let arg = eRStep3 tpe arg'
              in  if ErrorType == tpe
                    then UnsolvableType |< arg
                    else arg
              | (_, tpe) <- argTypes
              | arg' <- args'
            ]
functionDeclPass symStack is = (symStack, is)

buildArrayLiteral (ArrayLiteral pos exprs x) = ArrayLiteral pos (promoteTo sup <$> exprs) newX
    where
        newX = rStep3 (ArrayType (toInteger $ length exprs) sup) x
        exprTypes = eType <$> exprs
        sup = foldl1 (/\) exprTypes
buildArrayLiteral _ = "expression" `unexpectedIn` "buildArrayLiteral"

promoteTo tpe expr
    | tpe /= ErrorType,
      exprType `joinLeq` tpe =
        UnaryOp (position expr) Coercion expr $ newStep3 RightValue tpe
    | otherwise = expr
    where
        exprType = eType expr

promote expr1 expr2 = (promoteTo sup expr1, promoteTo sup expr2)
    where
        sup = eType expr1 \/ eType expr2

promoteList = zipWith promoteTo

checkTree = checkBlock (Status (pushEnv []) Nothing False)

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

checkBlock status@Status{symStack} block' =
    snd $ emapAccumLBlock (trace "foo12d7t" $ checkInstruction) status{symStack = newSymStack} block
    where
        (newSymStack, block) = trace "foo1" $ emapAccumLBlock (trace "foo000" $ functionDeclPass) symStack block'

checkInstruction :: Status -> Instruction Step -> (Status, Instruction Step)
checkInstruction status@Status{symStack} (VariableDecl pos id declType expr' x) =
    case solveResult of
        (False, ErrorType) -> (status, UnsolvableType |< variableDecl)
        _ -> case addBind symStack ModalityVal variableDecl of
            (Left (_, _, oldDecl)) -> (status, VariableAlreadyDefined (void oldDecl) |< variableDecl)
            (Right symStack) -> (status{symStack}, variableDecl)
    where
        tExpr = emap (checkExpr symStack) expr'
        solveResult@(ok, tpe) = solveVarDeclType declType (eType tExpr)
        expr
            | ok || ErrorType == tpe = tExpr
            | otherwise = TypeMismatch (eType expr) (Left tpe) |?< tExpr
        variableDecl = VariableDecl pos id declType (promoteTo tpe expr) (rStep3 tpe x)
checkInstruction status@Status{symStack} (FunctionDecl pos id args' declType block' x@Step3{sType = FunctionType argTypes retType}) =
    (status, FunctionDecl pos id args declType block x)
    where
        bindings =
            trace "foo4" $
                -- this was tragic mistake
                [ (modty, VariableDecl pos id declType (Id pos id (trace "foo5" $ newStep3 RightValue tpe)) (trace "foo5" $ newStep3 RightValue tpe)) -- binding has fdecl x
                  | ((modty, tpe), Param _ id declType x) <-
                        zip argTypes args'
                ]
        (newSymStack, args) = trace "foo6" $ mapAccumL (trace "foocwewwq" $ f) (pushEnv symStack) $ zip bindings args'
        block = trace "foo2002" $ checkBlock status{symStack = trace "foo7" $ newSymStack, function = Just retType, loop = False} block'
        f symStack (binding@(_, decl), arg)
            | ErrorType <- eType decl = (symStack, arg)
            | otherwise = case uncurry (addBind symStack) binding of
                Left (_, _, decl) -> (symStack, VariableAlreadyDefined (void decl) |< arg)
                Right symStack -> (symStack, arg)
checkInstruction status@Status{symStack} (Assignment pos expr1' op expr2' x) =
    case satisfiesAssignment op expr1 expr2 of
        (Nothing, Nothing) ->
            (status, Assignment pos expr1 op (promoteTo (eType expr1) expr2) (rStep3 VoidType x))
        (err1, err2) ->
            let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
            in  (status, Assignment pos (f err1 expr1) op (f err2 expr2) (rStep3 VoidType x))
    where
        expr1 = checkExpr symStack expr1'
        expr2 = checkExpr symStack expr2'
checkInstruction status@Status{symStack} (NestedBlock pos block' x) =
    (status, NestedBlock pos block (rStep3 VoidType x))
    where
        block = checkBlock status{symStack = pushEnv symStack} block'
checkInstruction status@Status{symStack} (While pos expr' block' x) =
    (status, While pos expr block (rStep3 VoidType x))
    where
        block = checkBlock status{symStack = pushEnv symStack, loop = True} block'
        expr = case checkExpr symStack expr' of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
checkInstruction status@Status{symStack} (IfThen pos expr' block' x) =
    (status, IfThen pos expr block (rStep3 VoidType x))
    where
        expr = case checkExpr symStack expr' of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
        block = checkBlock status{symStack = pushEnv symStack} block'
checkInstruction status@Status{symStack} (IfThenElse pos expr' block1' block2' x) =
    (status, IfThenElse pos expr block1 block2 (rStep3 VoidType x))
    where
        expr = case checkExpr symStack expr' of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
        block1 = checkBlock status{symStack = pushEnv symStack} block1'
        block2 = checkBlock status{symStack = pushEnv symStack} block2'
checkInstruction status@Status{loop} is@(Break{}) =
    (,) status $ case eRStep3 VoidType is of
        t | loop -> t
        t -> JumpOutsideLoop |< t
checkInstruction status@Status{loop} is@(Continue{}) =
    (,) status $ case eRStep3 VoidType is of
        t | loop -> t
        t -> JumpOutsideLoop |< t
checkInstruction status@Status{function} is@(ReturnVoid{}) = (,) status . eRStep3 VoidType $
    case function of
        Nothing -> ReturnOutsideFunction |< is
        Just tpe | ErrorType /= tpe, VoidType /= tpe -> TypeMismatch VoidType (Left tpe) |< is
        _ -> is
checkInstruction status@Status{symStack, function} (ReturnExp pos expr' x) = (,) status $
    case ReturnExp pos expr (rStep3 VoidType x) of
        t | Nothing <- function -> ReturnOutsideFunction |< t
        t -> t
    where
        expr = case checkExpr symStack expr' of
            t
                | Just fType <- function,
                  tpe <- eType t,
                  not (tpe `joinLeq` fType) ->
                    TypeMismatch tpe (Left fType) |?< t
            t
                | Just fType <- function ->
                    promoteTo fType t
            t -> t
checkInstruction status@Status{symStack} (Expression pos expr' x) =
    (status, Expression pos expr (rStep3 VoidType x))
    where
        expr = checkExpr symStack expr'
checkInstruction _ _ = "instruction" `unexpectedDuring` "checkInstruction"

-- Param Modality Ident (DeclType a) a
