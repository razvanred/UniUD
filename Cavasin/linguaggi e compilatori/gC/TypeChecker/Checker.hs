{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# HLINT ignore "Avoid partial function" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TypeChecker.Checker (checkTypes) where

import AST
import Algebra.Lattice (joinLeq, (\/))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either.Extra
import Data.List (find, findIndex)
import Data.Map.Strict (Map, insert, member, (!))
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumL)
-- import Debug.Trace (trace)

import TypeChecker.Intrinsics qualified as Intrinsics
import TypeChecker.TypeUtils
import Utils
import Prelude hiding (error, id)

data SymType = Variable | Function
    deriving (Eq, Ord, Show)

type SymEntry = (Int, Modality, Instruction Step)

type SymStack = [Map SymType (Map Ident SymEntry)]

data Status = Status
    { symStack :: SymStack,
      function :: Type,
      loop :: Bool,
      try :: Bool
    }

-- todo
-- exotic args modalities
-- array literal elimination with codegen - skip
-- graph cycle detection for function initializations/function captures -skip

-- requests:
-- every var symbol annotated with ref/value
-- every accessor must give lvalue, array valueness tracking must be done covertly
-- resolve deref pairs on leftSides statically

-- no indexed string access :c
-- no literals, except on initializations. it is what it is :c

rStep3 = flip (fillOutStep3 RightValue) Nothing

lStep3 = flip (fillOutStep3 LeftValue) Nothing

rBindStep3 = fillOutStep3 RightValue

eBindStep3 tpe side binding = pass1 updateAnn (fillOutStep3 side tpe (Just binding) . ann)

eRStep3 tpe = pass1 updateAnn (rStep3 tpe . ann)

eLStep3 tpe = pass1 updateAnn (lStep3 tpe . ann)

we |?< e
    | ErrorType <- eType e = e
    | TypeMismatch _ (Left ErrorType) <- we = e
    | otherwise = we |< e

instrinsics = symStack
    where
        fDecls = Intrinsics.intrinsicsDecls
        symStack = foldl f (pushEnv []) fDecls
        f symStack fDecl =
            case addBind symStack ModalityRef fDecl of
                Right symStack -> symStack
                Left binding -> error $ "failed to load intrinsics: " ++ show binding

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
    | (Id _ id _) <- decl = f id Variable
    | (FunctionCall _ id _ _) <- decl = f id Function
    | otherwise = "expression" `unexpectedIn` "queryBind"
    where
        f id symType = case find (\t -> id `member` (t ! symType)) symStack of
            Just symTable1 -> Just $ (symTable1 ! symType) ! id
            Nothing -> Nothing

pushEnv :: SymStack -> SymStack
pushEnv symStack = Map.fromList [(Variable, Map.empty), (Function, Map.empty)] : symStack

buildDeclType declType' = case solveVarDeclType declType' ErrorType of
    (_, tpe, declType) -> (tpe, declType)

solveVarDeclType declType initType = (ErrorType == accType, tpe, newDeclType)
    where
        tpe = cons VoidType
        ((_, cons, accType), newDeclType) = emapAccumLDeclType fDes (False, idty, initType) declType
        fDes (True, _, _) declType = (,) (True, const ErrorType, ErrorType) declType
        fDes (False, cons, initType) declType@(DPointerType _)
            | PointerType initType <- initType = (,) (False, cons . PointerType, initType) declType
            | otherwise = (,) (False, cons . PointerType, ErrorType) declType
        fDes (False, cons, initType) declType@(DArrayType chk Nothing _)
            | ArrayType _ len initType <- initType = (,) (False, cons . ArrayType chk len, initType) declType
            | otherwise = (,) (True, const ErrorType, ErrorType) declType
        fDes (False, cons, initType') (DArrayType chk (Just expr') declType) =
            case satisfiesTypeExpr expr of
                Nothing -> (,) (False, cons . ArrayType chk len1, initType) (DArrayType chk (Just $ promoteTo IntType expr) declType) -- TAC team said ok
                Just err -> (,) (True, const ErrorType, ErrorType) (DArrayType chk (Just $ err |?< expr) declType)
            where
                initType = case initType' of
                    (ArrayType _ len2 initType) | len2 == len1 -> initType
                    _ -> ErrorType
                len1 = litVal expr
                expr
                    | isLiteral expr' = step2ToStep3 <$> expr'
                    | otherwise = rStep3 ErrorType <$> expr'
                litVal (IntLiteral _ v _) = v
                litVal (CharLiteral _ v _) = fromIntegral $ fromEnum v
                litVal _ = "literal" `unexpectedDuring` "litVal"
        fDes (False, cons, initType) declType = (,) (False, cons . const (g declType), f initType declType) declType

        f initType declType
            | initType == g declType = initType
            | otherwise = ErrorType

        g DBoolType = BoolType
        g DCharType = CharType
        g DIntType = IntType
        g DStringType = StringType
        g DFloatType = FloatType
        g DVoidType = VoidType
        g _ = ErrorType

buildArrayLiteral (ArrayLiteral pos exprs x) = ArrayLiteral pos (promoteTo sup <$> exprs) newX
    where
        newX = rStep3 (ArrayType False (toInteger $ length exprs) sup) x
        exprTypes = eType <$> exprs
        sup = foldl1 (\/) exprTypes
buildArrayLiteral _ = "expression" `unexpectedIn` "buildArrayLiteral"

promoteTo tpe expr
    | exprType `joinLe` tpe,
      ErrorType /= tpe =
        UnaryOp (position expr) Coercion expr $ newStep3 RightValue tpe
    | otherwise = expr
    where
        exprType = eType expr

promote expr1 expr2 = (promoteTo sup expr1, promoteTo sup expr2)
    where
        sup = eType expr1 \/ eType expr2

promoteList = zipWith promoteTo

-- checking

checkTypes = checkBlock (Status instrinsics VoidType False False)

functionDeclPass symStack (FunctionDecl pos id args' return' block x) =
    case addBind symStack ModalityRef fDecl of
        Left (_, _, oldFDecl) -> (symStack, FunctionAlreadyDefined (void oldFDecl) |< fDecl)
        Right symStack -> (symStack, fDecl)
    where
        (retType, ret) = buildDeclType return'
        (argTypes, args) =
            unzip
                [ ( (modty, tpe),
                    case tpe of
                        ErrorType -> UnsolvableType |< arg
                        _ -> arg
                  )
                  | (Param pos modty id argDecl' x) <- args',
                    let (tpe, argDecl) = buildDeclType argDecl',
                    let arg = Param pos modty id argDecl (lStep3 tpe x)
                ]
        finalType
            | ErrorType `notElem` (snd <$> argTypes) = retType
            | otherwise = ErrorType
        fDecl = case FunctionDecl pos id args ret block (rStep3 (FunctionType argTypes retType) x) of
            t | ErrorType <- finalType -> UnsolvableType |< t
            t | ArrayType{} <- retType -> DisallowedType |< t
            t | StringType <- retType -> DisallowedType |< t
            t -> t
functionDeclPass symStack is = (symStack, is)

checkExpr symStack = emap check
    where
        disallowLiteral expr = if isCompLiteral expr then DisallowedLiteral |< expr else expr
        check :: Expr Step -> Expr Step
        check expr
            | Step2{} <- ann expr = updateAnn (step2ToStep3 $ ann expr) expr -- literals
        check ident@(Id{}) = case queryBind symStack ident of -- TODO get valueness from declaration
            Just binding@(_, _, decl) -> eBindStep3 (eType decl) (eSide decl) binding ident
            Nothing -> UnknownSymbol |< eLStep3 ErrorType ident
        check expr@(UnaryOp pos op subExpr x) =
            case satisfiesUnOp op subExpr of
                Nothing -> eRStep3 (eType subExpr) expr
                Just (got, expected) -> UnaryOp pos op (TypeMismatch got expected |?< subExpr) (rStep3 ErrorType x)
        check (BinaryOp pos op subExpr1 subExpr2 x) =
            case satisfiesBinOp op subExpr1 subExpr2 of
                (Nothing, Nothing) ->
                    let sup = eType subExpr1 \/ eType subExpr2
                        (newSubExpr1, newSubExpr2) = promote subExpr1 subExpr2
                        tpe
                            | ArithmeticOp{} <- op = sup
                            | otherwise = BoolType
                    in  BinaryOp pos op newSubExpr1 newSubExpr2 (rStep3 tpe x)
                (err1, err2) ->
                    let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                    in  BinaryOp pos op (f err1 subExpr1) (f err2 subExpr2) (rStep3 ErrorType x)
        check expr@(Ref pos subExpr x) =
            case satisfiesRef subExpr of
                Nothing -> eRStep3 (pushPointer $ eType subExpr) expr
                Just (got, expected) -> Ref pos (TypeMismatch got expected |?< subExpr) (rStep3 ErrorType x)
        check expr@(Deref pos subExpr x) =
            case satisfiesDeref subExpr of
                Nothing -> eLStep3 (popPointer $ eType subExpr) expr
                Just (got, expected) -> Deref pos (TypeMismatch got expected |?< subExpr) (lStep3 ErrorType x)
        check (ArrayAcc pos subExpr indExpr x) =
            case satisfiesAccessor subExpr indExpr of
                (Nothing, Nothing) ->
                    ArrayAcc pos (disallowLiteral subExpr) (promoteTo IntType indExpr) (fillOutStep3 (eSide subExpr) (popArray $ eType subExpr) Nothing x)
                (err1, err2) ->
                    let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                    in  ArrayAcc pos (f err1 subExpr) (f err2 indExpr) (fillOutStep3 (eSide subExpr) ErrorType Nothing x)
        check fcall@(FunctionCall pos id subExprs x) =
            case queryBind symStack fcall of
                Just binding@(_, _, decl) ->
                    case satisfiesFCall fType subExprs of
                        (False, argErrors)
                            | Nothing <- foldl (<|>) Nothing argErrors ->
                                FunctionCall pos id (promoteList (snd <$> argTypes) (disallowLiteral <$> subExprs)) (rBindStep3 retType (Just binding) x)
                        (argCount, argErrors) ->
                            let newX = if argCount then ArgCount |< x else x
                            in  FunctionCall pos id (f <$> argErrors <*> subExprs) (rBindStep3 ErrorType (Just binding) newX)
                    where
                        fType = eType decl
                        (FunctionType argTypes retType) = fType
                        f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |?<)
                Nothing -> UnknownSymbol |< eRStep3 ErrorType fcall
        check expr@(ArrayLiteral{}) =
            case satisfiesArrayLiteral expr of
                (False, False) -> buildArrayLiteral expr
                t ->
                    let newExpr = eRStep3 ErrorType expr
                    in  case t of
                            (True, False) -> EmptyArray |< newExpr
                            (False, True) -> UnsolvableType |< newExpr
                            (True, True) -> newExpr
        check (CondExpr pos subExpr subExpr1 subExpr2 x) =
            case satisfiesCondExpr subExpr subExpr1 subExpr2 of
                (0, Nothing) ->
                    CondExpr pos subExpr (promoteTo sup (disallowLiteral subExpr1)) (promoteTo sup (disallowLiteral subExpr2)) (rStep3 sup x)
                (n, condErr) ->
                    let expr = CondExpr pos (maybe subExpr (\(got, expected) -> TypeMismatch got expected |?< subExpr) condErr) subExpr1 subExpr2 (rStep3 ErrorType x)
                    in  case n of
                            1 -> UnsolvableType |< expr
                            _ -> expr
            where
                sup = eType subExpr1 \/ eType subExpr2
        check expr = "expression " ++ show expr `unexpectedDuring` "checkExpr"

checkBlock status@Status{symStack} block' =
    snd $ emapAccumLBlock checkInstruction status{symStack = newSymStack} block
    where
        (newSymStack, block) = emapAccumLBlock functionDeclPass symStack block'

checkInstruction :: Status -> Instruction Step -> (Status, Instruction Step)
checkInstruction status@Status{symStack} (VariableDecl pos id declType' expr' x) =
    case addBind symStack ModalityVal variableDecl of
        (Left (_, _, oldDecl)) -> (status, VariableAlreadyDefined (void oldDecl) |< variableDecl)
        (Right symStack) -> (status{symStack}, variableDecl)
    where
        tExpr = checkExpr symStack expr'
        tExprType = eType tExpr
        solveResult@(error, tpe, declType) = solveVarDeclType declType' tExprType
        expr
            | error && ErrorType /= tpe && tExprType `notJoinLeq` tpe = TypeMismatch (eType expr) (Left tpe) |?< tExpr
            | otherwise = tExpr
        variableDecl = case VariableDecl pos id declType (promoteTo tpe expr) (lStep3 tpe x) of
            t | (False, ErrorType, _) <- solveResult -> UnsolvableType |< t
            t -> t
checkInstruction status@Status{symStack} (FunctionDecl pos id args' declType block' x@Step3{sType = FunctionType argTypes retType}) =
    (status, FunctionDecl pos id args declType block x)
    where
        bindings =
            -- this was tragic mistake
            [ (modty, VariableDecl pos id declType (Id pos id x) x) -- binding has param x
              | ((modty, _), Param pos _ id declType x) <-
                    zip argTypes args'
            ]
        (newSymStack, args) = mapAccumL f (pushEnv symStack) $ zip bindings args'
        block = checkBlock status{symStack = newSymStack, function = retType, loop = False} block'
        f symStack (binding, arg) =
            case uncurry (addBind symStack) binding of
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
checkInstruction status@Status{symStack} (DoWhile pos expr' block' x) =
    (status, DoWhile pos expr block (rStep3 VoidType x))
    where
        block = checkBlock status{symStack = pushEnv symStack, loop = True} block'
        expr = case checkExpr symStack expr' of
            t | BoolType <- eType t -> t
            t -> TypeMismatch (eType t) (Left BoolType) |?< t
checkInstruction status@Status{symStack} (For pos id fromExpr' toExpr' incrExpr' block' x) =
    (status, For pos id fromExpr toExpr incrExpr block (rStep3 VoidType x))
    where
        fromExpr = case checkExpr symStack fromExpr' of
            t | eType t `joinLeq` IntType -> promoteTo IntType t
            t -> TypeMismatch (eType t) (Right "Integral") |?< t
        toExpr = case checkExpr symStack toExpr' of
            t | eType t `joinLeq` IntType -> promoteTo IntType t
            t -> TypeMismatch (eType t) (Right "Integral") |?< t
        incrExpr = case checkExpr symStack incrExpr' of
            t | eType t `joinLeq` IntType -> promoteTo IntType t
            t -> TypeMismatch (eType t) (Right "Integral") |?< t
        binding = (ModalityVal, VariableDecl pos id DIntType (eToRValue fromExpr) (newStep3 RightValue IntType))
        newSymStack = case uncurry (addBind (pushEnv symStack)) binding of
            Left _ -> "bound accumulator " ++ id `unexpectedIn` "newSymStack"
            Right symStack -> symStack
        block = checkBlock status{symStack = newSymStack, loop = False} block'
        eToRValue e = updateAnn (ann e){sSide = RightValue} e
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
checkInstruction status@Status{symStack} (TryCatch pos block1' block2' x) =
    (status, TryCatch pos block1 block2 (rStep3 VoidType x))
    where
        block1 = checkBlock status{symStack = pushEnv symStack, try = True} block1'
        block2 = checkBlock status{symStack = pushEnv symStack} block2'
checkInstruction status@Status{loop} is@(Break{}) =
    (,) status $ case eRStep3 VoidType is of
        t | loop -> t
        t -> JumpOutsideLoop |< t
checkInstruction status@Status{loop} is@(Continue{}) =
    (,) status $ case eRStep3 VoidType is of
        t | loop -> t
        t -> JumpOutsideLoop |< t
checkInstruction status@Status{function} is@(ReturnVoid{}) =
    (,) status . eRStep3 VoidType $
        if ErrorType /= function && VoidType /= function
            then
                TypeMismatch VoidType (Left function) |< is
            else
                is
checkInstruction status@Status{try} is@(Throw{}) =
    (,) status $ case eRStep3 VoidType is of
        t | try -> t
        t -> ThrowOutsideTry |< t
checkInstruction status@Status{symStack, function} (ReturnExp pos expr' x) =
    (,) status $ ReturnExp pos expr (rStep3 VoidType x)
    where
        expr = case checkExpr symStack expr' of
            t
                | tpe <- eType t,
                  tpe `notJoinLeq` function ->
                    TypeMismatch tpe (Left function) |?< t
            t -> promoteTo function t
checkInstruction status@Status{symStack} (Expression pos expr' x) =
    (status, Expression pos expr (rStep3 VoidType x))
    where
        expr = checkExpr symStack expr'
checkInstruction _ _ = "instruction" `unexpectedDuring` "checkInstruction"
