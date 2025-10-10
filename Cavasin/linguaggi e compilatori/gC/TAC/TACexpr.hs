{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TAC.TACexpr where

import AST
import Control.Monad.Trans.State
import Data.Map.Strict qualified as Map
import TAC.TAC
import TAC.TACutils

tacExpr :: Expr ErrorCollectorOutput -> Stato XAddr
tacExpr (UnaryOp pos PreDecr expr (ErrorCollectorOutput t _ _ _)) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            outWithSuspendedLabel $ TacBinary addr (ArithmeticOp Sub) t addr (buildTacLiteralOneType t)
            return $ Addr addr
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            outWithSuspendedLabel $ TacBinary (addr xtmp) (ArithmeticOp Sub) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacPointerStore a t (addr xtmp)
            return xtmp
        (ArrayAddr addr1 addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr
            outWithSuspendedLabel $ TacBinary (addr xtmp) (ArithmeticOp Sub) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacIndexedStore xaddr t (addr xtmp)
            return xtmp
tacExpr (UnaryOp pos PreIncr expr (ErrorCollectorOutput t _ _ _)) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            outWithSuspendedLabel $ TacBinary addr (ArithmeticOp Add) t addr (buildTacLiteralOneType t)
            return $ Addr addr
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            outWithSuspendedLabel $ TacBinary (addr xtmp) (ArithmeticOp Add) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacPointerStore a t (addr xtmp)
            return xtmp
        (ArrayAddr addr1 addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr
            outWithSuspendedLabel $ TacBinary (addr xtmp) (ArithmeticOp Add) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacIndexedStore xaddr t (addr xtmp)
            return xtmp
tacExpr (UnaryOp pos PostDecr expr (ErrorCollectorOutput t _ _ _)) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacNullary temp t addr
            outWithSuspendedLabel $ TacBinary addr (ArithmeticOp Sub) t addr (buildTacLiteralOneType t)
            return $ Addr temp
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            f1 <- newtemp
            let temp2 = f1 t
            outWithSuspendedLabel $ TacBinary temp2 (ArithmeticOp Sub) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacPointerStore a t temp2
            return xtmp
        (ArrayAddr addr1 addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Sub) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacIndexedStore xaddr t (addr xtmp1)
            return xtmp
tacExpr (UnaryOp pos PostIncr expr (ErrorCollectorOutput t _ _ _)) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacNullary temp t addr
            outWithSuspendedLabel $ TacBinary addr (ArithmeticOp Add) t addr (buildTacLiteralOneType t)
            return $ Addr temp
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            f1 <- newtemp
            let temp2 = f1 t
            outWithSuspendedLabel $ TacBinary temp2 (ArithmeticOp Add) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacPointerStore a t temp2
            return xtmp
        (ArrayAddr addr1 addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Add) t (addr xtmp) (buildTacLiteralOneType t)
            outWithSuspendedLabel $ TacIndexedStore xaddr t (addr xtmp1)
            return xtmp
tacExpr expr@(UnaryOp pos Not _ _) = boolExprAux expr (buildTacLiteral . TacLitBool $ True) (buildTacLiteral . TacLitBool $ False)
tacExpr (UnaryOp pos uop expr (ErrorCollectorOutput t _ _ _)) = do
    f <- newtemp
    let temp = f t
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            outWithSuspendedLabel $ TacUnary temp uop t addr
            return $ Addr temp
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            outWithSuspendedLabel $ TacUnary temp uop t (addr xtmp)
            return xtmp
tacExpr expr@(BinaryOp _ (RelationalOp NotEq) expr1 expr2 _) = do
    case (extractTypeFromExpr expr1, extractTypeFromExpr expr2) of
        (StringType, StringType) -> do
            let mockParam = Param (0, 0) ModalityVal "" DVoidType ()
            let mockParamList = [mockParam, mockParam]
            let mockFunctionDecl = Just (FunctionDecl (0, 0) "" mockParamList DVoidType [] ())
            let mockAnnotedField = ErrorCollectorOutput StringType Nothing Nothing mockFunctionDecl
            let funCallexpr = FunctionCall (0, 0) "stringEq" [expr1, expr2] mockAnnotedField
            boolExprAuxCustom funCallexpr (buildTacLiteral . TacLitBool $ False) (buildTacLiteral . TacLitBool $ True)
        _ -> boolExprAux expr (buildTacLiteral . TacLitBool $ True) (buildTacLiteral . TacLitBool $ False)
tacExpr expr@(BinaryOp _ (RelationalOp Eq) expr1 expr2 _) = do
    case (extractTypeFromExpr expr1, extractTypeFromExpr expr2) of
        (StringType, StringType) -> do
            let mockParam = Param (0, 0) ModalityVal "" DVoidType ()
            let mockParamList = [mockParam, mockParam]
            let mockFunctionDecl = Just (FunctionDecl (0, 0) "" mockParamList DVoidType [] ())
            let mockAnnotedField = ErrorCollectorOutput StringType Nothing Nothing mockFunctionDecl
            tacExpr (FunctionCall (0, 0) "stringEq" [expr1, expr2] mockAnnotedField)
        _ -> boolExprAux expr (buildTacLiteral . TacLitBool $ True) (buildTacLiteral . TacLitBool $ False)
tacExpr expr@(BinaryOp _ (BooleanOp _) _ _ _) = boolExprAux expr (buildTacLiteral . TacLitBool $ True) (buildTacLiteral . TacLitBool $ False)
tacExpr (BinaryOp pos bop expr1 expr2 (ErrorCollectorOutput t _ _ _)) = do
    xaddr1 <- tacExpr expr1
    xaddr2 <- tacExpr expr2
    case (xaddr1, xaddr2) of
        (Addr addr1, Addr addr2) -> buildTempANDTacBinary bop t addr1 addr2
        (RefAddr addr1, Addr addr2) -> do
            xtmp <- buildTempANDIndirectLoad addr1 t False
            buildTempANDTacBinary bop t (addr xtmp) addr2
        (Addr addr1, RefAddr addr2) -> do
            xtmp <- buildTempANDIndirectLoad addr2 t False
            buildTempANDTacBinary bop t addr1 (addr xtmp)
        (RefAddr addr1, RefAddr addr2) -> do
            xtmp1 <- buildTempANDIndirectLoad addr1 t False
            xtmp2 <- buildTempANDIndirectLoad addr2 t False
            buildTempANDTacBinary bop t (addr xtmp1) (addr xtmp2)
tacExpr (Ref _ _ (ErrorCollectorOutput _ (Just LeftValue) _ _)) = error "& l-expr mjst produce a r-value!"
tacExpr (Ref _ _ (ErrorCollectorOutput _ Nothing _ _)) = error "& l-expr mjst produce a r-value!"
tacExpr (Ref _ expr (ErrorCollectorOutput t (Just RightValue) _ _)) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr a) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacReferenceLoad temp t a
            return $ Addr temp
        (RefAddr a) -> return . Addr $ a{addrT = t}
        (ArrayAddr base offset) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacReferenceLoad temp t base
            buildTempANDPointerTacBinary t temp offset
tacExpr (Deref _ expr (ErrorCollectorOutput t lr _ _)) = do
    xaddr <- tacExpr expr
    case (xaddr, lr) of
        (Addr a, Just LeftValue) -> return $ RefAddr a{addrT = t}
        (RefAddr a, _) -> buildTempANDIndirectLoad a (addrT a) True
        (Addr a, Just RightValue) -> buildTempANDIndirectLoad a (addrT a) False
        (ArrayAddr b o, _) -> do
            case t of
                ArrayType{} -> do
                    xtmp <- buildTempANDTacIndexedLoad (getPrimitiveTypeArr t) xaddr
                    return $ RefAddr (addr xtmp)
                _ -> do
                    xtmp <- buildTempANDTacIndexedLoad t xaddr
                    return $ RefAddr (addr xtmp)
tacExpr (ArrayAcc pos expr1 expr2 (ErrorCollectorOutput t lr _ _)) = do
    xaddr1 <- tacExpr expr1
    xaddr2 <- tacExpr expr2
    let bt@(ArrayType flag dim ty) = extractTypeFromExpr expr1
    case (xaddr1, xaddr2) of
        (Addr a1, Addr a2) -> do
            checkDim flag dim a2
            case a2 of
                TacLit _ _ -> do
                    let contentLiterala2 = contentInt . tacLit $ a2
                    let newOffsetLiteral = buildTacLiteral . TacLitInt . (*) contentLiterala2 . sizeof $ ty
                    case lr of
                        Just LeftValue -> return $ ArrayAddr a1 newOffsetLiteral
                        Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 newOffsetLiteral)
                        Nothing -> error "should be left/right value"
                _ -> do
                    xtmp <- buildTempANDTacBinary (ArithmeticOp Mul) IntType a2 (buildTacLiteral . TacLitInt . sizeof $ ty)
                    case lr of
                        Just LeftValue -> return $ ArrayAddr a1 (addr xtmp)
                        Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 (addr xtmp))
                        Nothing -> error "should be left/right value"
        (ArrayAddr a1 partialOffset, Addr a2) -> do
            checkDim flag dim a2
            case (partialOffset, a2) of
                (TacLit _ _, TacLit _ _) -> do
                    let contentLiterala2 = contentInt . tacLit $ a2
                    let contentLiteralPartialOff = contentInt . tacLit $ partialOffset
                    let newOffsetLiteral = buildTacLiteral . TacLitInt . (+) contentLiteralPartialOff . (*) contentLiterala2 . sizeof $ ty
                    case lr of
                        Just LeftValue -> return $ ArrayAddr a1 newOffsetLiteral
                        Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 newOffsetLiteral)
                        Nothing -> error "should be left/right value"
                _ -> do
                    xtmp1 <- buildTempANDTacBinary (ArithmeticOp Mul) IntType a2 (buildTacLiteral . TacLitInt . sizeof $ ty)
                    xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType (addr xtmp1) partialOffset
                    case lr of
                        Just LeftValue -> return $ ArrayAddr a1 (addr xtmp2)
                        Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 (addr xtmp2))
                        Nothing -> error "should be left/right value"
        (Addr a1, RefAddr a2) -> do
            xtmp <- buildTempANDIndirectLoad a2 IntType False
            checkDim flag dim (addr xtmp)
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Mul) IntType (addr xtmp) (buildTacLiteral . TacLitInt . sizeof $ ty)
            case lr of
                Just LeftValue -> return $ ArrayAddr a1 (addr xtmp1)
                Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 (addr xtmp1))
                Nothing -> error "should be left/right value"
        (ArrayAddr a1 partialOffset, RefAddr a2) -> do
            xtmp <- buildTempANDIndirectLoad a2 IntType False
            checkDim flag dim (addr xtmp)
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Mul) IntType (addr xtmp) (buildTacLiteral . TacLitInt . sizeof $ ty)
            xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType (addr xtmp1) partialOffset
            case lr of
                Just LeftValue -> return $ ArrayAddr a1 (addr xtmp2)
                Just RightValue -> buildTempANDTacIndexedLoad t (ArrayAddr a1 (addr xtmp2))
                Nothing -> error "should be left/right value"
        (RefAddr a1, Addr a2) -> do
            checkDim flag dim a2
            case a2 of
                TacLit _ _ -> do
                    let contentLiterala2 = contentInt . tacLit $ a2
                    let newOffsetLiteral = buildTacLiteral . TacLitInt . (*) contentLiterala2 . sizeof $ ty
                    xtmp <- buildTempANDPointerTacBinary (PointerType . getPrimitiveTypeArr $ bt) a1 newOffsetLiteral
                    return . RefAddr . addr $ xtmp
                _ -> do
                    xtmp <- buildTempANDTacBinary (ArithmeticOp Mul) IntType a2 (buildTacLiteral . TacLitInt . sizeof $ ty)
                    xtmp1 <- buildTempANDPointerTacBinary (PointerType . getPrimitiveTypeArr $ bt) a1 (addr xtmp)
                    return . RefAddr . addr $ xtmp1
        (RefAddr a1, RefAddr a2) -> do
            xtmp <- buildTempANDIndirectLoad a2 IntType False
            checkDim flag dim (addr xtmp)
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Mul) IntType (addr xtmp) (buildTacLiteral . TacLitInt . sizeof $ ty)
            xtmp2 <- buildTempANDPointerTacBinary (PointerType . getPrimitiveTypeArr $ bt) a1 (addr xtmp1)
            return . RefAddr . addr $ xtmp2
tacExpr (Id _ _ (ErrorCollectorOutput _ _ _ Nothing)) = error "An Identifier must have a declaration information"
tacExpr (Id _ _ (ErrorCollectorOutput _ _ Nothing _)) = error "An Identifier must have a modality"
tacExpr (Id _ identifier (ErrorCollectorOutput t _ (Just m) (Just decl))) = case m of
    mv@ModalityVal -> return $ Addr{addr = ProgVar{progVar = buildProgVariable identifier (getLocFromDecl decl) t mv, addrT = t}}
    mref@ModalityRef -> return $ RefAddr{addr = ProgVar{progVar = buildProgVariable identifier (getLocFromDecl decl) t mref, addrT = t}}
    mres@ModalityRes -> return $ Addr{addr = ProgVar{progVar = buildProgVariable (identifier ++ "_localcopy") (getLocFromDecl decl) t mres, addrT = t}}
    mvr@ModalityValRes -> return $ RefAddr{addr = ProgVar{progVar = buildProgVariable (identifier ++ "_localcopy") (getLocFromDecl decl) t mvr, addrT = t}}
tacExpr (FunctionCall _ id exprs (ErrorCollectorOutput t _ _ (Just decl))) = do
    let funid = FunId (getLocFromDecl decl) id (length exprs)
    tacActualParameters exprs (getParamList decl)
    f <- newtemp
    let temp = f t
    outWithSuspendedLabel $ TacCall (Just (t, temp)) funid
    return . Addr $ temp
-- consideriamo t il tipo di ritorno della funzione determinato in fase di type checking
tacExpr (FunctionCall _ _ _ (ErrorCollectorOutput VoidType _ m Nothing)) = error "There cannot be an empty function declaration information"
tacExpr (FunctionCall _ _ _ (ErrorCollectorOutput VoidType _ m (Just (FunctionDecl _ _ _ DVoidType _ _)))) = error "There cannot be a procedure in a Expr context"
-- noi non vedremo funzioni che tornano array, perché verranno modificate in analisi semantica statica
tacExpr (FunctionCall _ id exprs (ErrorCollectorOutput t _ _ Nothing)) = error ""
tacExpr (IntLiteral _ i _) = return $ Addr{addr = buildTacLiteral (TacLitInt i)}
tacExpr (CharLiteral _ c _) = return $ Addr{addr = buildTacLiteral (TacLitChar c)}
tacExpr (FloatLiteral _ f _) = return $ Addr{addr = buildTacLiteral (TacLitFloat f)}
tacExpr (BoolLiteral _ b _) = return $ Addr{addr = buildTacLiteral (TacLitBool b)}
tacExpr (StringLiteral pos str _) = do
    let terminatedStr = str ++ "\0"
    (b, k, j, labels, tac, suspendedLabels, bclabels, strings) <- get
    case Map.lookup terminatedStr strings of
        Nothing -> do
            let stringRef = "stringPtr" ++ show j
            let stringProgVar = ProgVar{progVar = buildProgVariable stringRef pos (PointerType CharType) ModalityVal, addrT = PointerType CharType}
            let adjStrings = Map.insert terminatedStr stringRef strings
            put (b, k, j + 1, labels, tac, suspendedLabels, bclabels, adjStrings)
            return . Addr $ stringProgVar
        Just stringRef -> do
            let stringProgVar = ProgVar{progVar = buildProgVariable stringRef pos (PointerType CharType) ModalityVal, addrT = PointerType CharType}
            return . Addr $ stringProgVar
tacExpr (CondExpr pos guardExpr expr2 expr3 (ErrorCollectorOutput t _ _ _)) = do
    int <- newLabelNum
    let label = LabIfFalse int
    f <- newtemp
    let temp = f t
    tacBoolExpr guardExpr FALL label
    xaddrLeft <- tacExpr expr2
    outWithSuspendedLabel $ TacNullary temp t (addr xaddrLeft)
    int2 <- newLabelNum
    let label2 = LabInstr int2
    outWithSuspendedLabel $ TacUnCondJump label2
    labelNext label
    xaddrRight <- tacExpr expr3
    outWithSuspendedLabel $ TacNullary temp t (addr xaddrRight)
    labelNext label2
    return . Addr $ temp

tacArrInit :: Addr -> Addr -> [Expr ErrorCollectorOutput] -> Type -> Stato ()
tacArrInit _ _ [] _ = return ()
tacArrInit address offset@(TacLit (TacLitInt i) _) (x : xs) t = do
    xaddr <- tacExpr x
    case xaddr of
        (Addr a) -> do
            outWithSuspendedLabel $ TacIndexedStore (ArrayAddr address offset) t a
            tacArrInit address (buildTacLiteral . TacLitInt $ i + sizeof t) xs t
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            outWithSuspendedLabel $ TacIndexedStore (ArrayAddr address offset) t (addr xtmp)
            tacArrInit address (buildTacLiteral . TacLitInt $ i + sizeof t) xs t

copyActualArray :: XAddr -> Type -> Addr -> Integer -> Stato ()
copyActualArray from baseType offset 0 = return ()
copyActualArray from baseType offset size = do
    f <- newtemp
    let temp = f baseType
    case from of
        (RefAddr a) -> do
            xtmp1 <- buildTempANDPointerTacBinary (PointerType baseType) a offset
            outWithSuspendedLabel $ TacPointerLoad temp baseType (addr xtmp1)
            outWithSuspendedLabel $ TacParam baseType temp
            let contentOffset = contentInt . tacLit $ offset
            let newoffset = buildTacLiteral . TacLitInt . (+) contentOffset . sizeof $ baseType
            copyActualArray from baseType newoffset (size - sizeof baseType)
        _ -> do
            outWithSuspendedLabel $ TacIndexedLoad temp baseType (ArrayAddr (addr from) offset)
            outWithSuspendedLabel $ TacParam baseType temp
            let sizeBaseType = buildTacLiteral . TacLitInt . sizeof $ baseType
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Add) IntType offset sizeBaseType
            copyActualArray from baseType (addr xtmp1) (size - sizeof baseType)

tacActualParameters :: [Expr ErrorCollectorOutput] -> [Parameter a] -> Stato ()
tacActualParameters [] _ = return ()
tacActualParameters esp@(e : es) pars@((Param _ m _ _ _) : ps) = do
    xaddr <- tacExpr e
    let t = extractTypeFromExpr e
    let pt = PointerType t
    case (m, t, xaddr) of
        (ModalityVal, ArrayType{}, Addr a) -> copyActualArray (Addr a) (getPrimitiveTypeArr t) (buildTacLiteral . TacLitInt $ 0) (sizeof t)
        (ModalityVal, ArrayType{}, RefAddr a) -> copyActualArray (RefAddr a) (getPrimitiveTypeArr t) (buildTacLiteral . TacLitInt $ 0) (sizeof t)
        (ModalityVal, ArrayType{}, ArrayAddr b o) -> copyActualArray (Addr b) (getPrimitiveTypeArr t) o (sizeof t)
        (ModalityVal, _, Addr a) -> outWithSuspendedLabel $ TacParam t a
        (ModalityVal, _, RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            outWithSuspendedLabel $ TacParam t (addr xtmp)
        (ModalityVal, _, ArrayAddr b o) -> error "should not be the case"
        (ModalityRef, _, Addr a) -> do
            f <- newtemp
            let temp = f pt
            outWithSuspendedLabel $ TacReferenceLoad temp pt a
            outWithSuspendedLabel $ TacParam pt temp
        (ModalityRef, _, RefAddr a) -> outWithSuspendedLabel $ TacParam pt a
        (ModalityRef, _, ArrayAddr b o) -> do
            f <- newtemp
            let temp = f pt
            outWithSuspendedLabel $ TacReferenceLoad temp pt b
            xtmp1 <- buildTempANDPointerTacBinary pt temp o
            outWithSuspendedLabel $ TacParam pt (addr xtmp1)
    tacActualParameters es ps

tacBoolExpr :: Expr ErrorCollectorOutput -> Label -> Label -> Stato ()
tacBoolExpr bs@BoolLiteral{} btrue bfalse = do
    lit <- tacExpr bs
    let content = contentBool . tacLit . addr $ lit
    case (btrue, bfalse, content) of
        (FALL, FALL, _) -> return ()
        (FALL, _, True) -> return ()
        (FALL, _, False) -> outWithSuspendedLabel $ TacUnCondJump bfalse
        (_, FALL, True) -> outWithSuspendedLabel $ TacUnCondJump btrue
        (_, FALL, False) -> return ()
        (_, _, True) -> outWithSuspendedLabel $ TacUnCondJump btrue
        (_, _, False) -> outWithSuspendedLabel $ TacUnCondJump bfalse
tacBoolExpr ident@(Id _ id _) btrue bfalse = do
    a <- resolveIdAux ident
    case (btrue, bfalse) of
        (FALL, FALL) -> return ()
        (FALL, _) -> outWithSuspendedLabel $ TacBoolCondJump False a bfalse
        (_, FALL) -> outWithSuspendedLabel $ TacBoolCondJump True a btrue
        (_, _) -> do
            outWithSuspendedLabel $ TacBoolCondJump False a bfalse
            outWithSuspendedLabel $ TacBoolCondJump True a btrue
tacBoolExpr (UnaryOp _ Not expr _) btrue bfalse = tacBoolExpr expr bfalse btrue
tacBoolExpr (BinaryOp _ (BooleanOp And) b1 b2 _) btrue bfalse = do
    case bfalse of
        FALL -> do
            numId <- newLabelNum
            let newLabel = LabFalseAnd numId
            tacBoolExpr b1 FALL newLabel
            tacBoolExpr b2 btrue bfalse
            labelNext newLabel
        _ -> do
            tacBoolExpr b1 FALL bfalse
            tacBoolExpr b2 btrue bfalse
tacBoolExpr (BinaryOp _ (BooleanOp Or) b1 b2 _) btrue bfalse = do
    case btrue of
        FALL -> do
            numId <- newLabelNum
            let newlabel = LabTrueOr numId
            tacBoolExpr b1 newlabel FALL
            tacBoolExpr b2 btrue bfalse
            labelNext newlabel
        _ -> do
            tacBoolExpr b1 btrue FALL
            tacBoolExpr b2 btrue bfalse
tacBoolExpr (BinaryOp _ (RelationalOp rel) e1 e2 _) btrue bfalse = do
    (a1, a2) <- resolveExprAux e1 e2
    let oppRel = oppositeRel rel
    case (btrue, bfalse) of
        (FALL, FALL) -> return ()
        (FALL, _) -> outWithSuspendedLabel $ TacRelCondJump a1 oppRel (addrT a1) a2 bfalse
        (_, FALL) -> outWithSuspendedLabel $ TacRelCondJump a1 rel (addrT a1) a2 btrue
        (_, _) -> do
            outWithSuspendedLabel $ TacRelCondJump a1 rel (addrT a1) a2 btrue
            outWithSuspendedLabel $ TacUnCondJump bfalse

resolveExprAux :: Expr ErrorCollectorOutput -> Expr ErrorCollectorOutput -> Stato (Addr, Addr)
resolveExprAux e1 e2 = do
    xaddr1 <- tacExpr e1
    xaddr2 <- tacExpr e2
    case (xaddr1, xaddr2) of
        (Addr a1, Addr a2) -> return (a1, a2)
        (Addr a1, RefAddr a2) -> do
            xtmp2 <- buildTempANDIndirectLoad a2 (addrT a2) False
            return (a1, addr xtmp2)
        (RefAddr a1, Addr a2) -> do
            xtmp1 <- buildTempANDIndirectLoad a1 (addrT a1) False
            return (addr xtmp1, a2)
        (RefAddr a1, RefAddr a2) -> do
            xtmp1 <- buildTempANDIndirectLoad a1 (addrT a1) False
            xtmp2 <- buildTempANDIndirectLoad a2 (addrT a2) False
            return (addr xtmp1, addr xtmp2)

resolveIdAux :: Expr ErrorCollectorOutput -> Stato Addr
resolveIdAux id = do
    xaddr <- tacExpr id
    case xaddr of
        (Addr a) -> return a
        (RefAddr a) -> do
            xtmp <- buildTempANDIndirectLoad a (addrT a) False
            return . addr $ xtmp

boolExprAux :: Expr ErrorCollectorOutput -> Addr -> Addr -> Stato XAddr
boolExprAux expr addr1 addr2 = do
    int <- newLabelNum
    let label = LabIfFalse int
    f <- newtemp
    let temp = f BoolType
    tacBoolExpr expr FALL label
    outWithSuspendedLabel $ TacNullary temp BoolType addr1
    int2 <- newLabelNum
    let label2 = LabInstr int2
    outWithSuspendedLabel $ TacUnCondJump label2
    labelNext label
    outWithSuspendedLabel $ TacNullary temp BoolType addr2
    labelNext label2
    return . Addr $ temp

boolExprAuxCustom :: Expr ErrorCollectorOutput -> Addr -> Addr -> Stato XAddr
boolExprAuxCustom expr addr1 addr2 = do
    int <- newLabelNum
    let label = LabIfFalse int
    f <- newtemp
    let temp = f BoolType
    funCall <- tacExpr expr
    outWithSuspendedLabel $ TacBoolCondJump False (addr funCall) label
    outWithSuspendedLabel $ TacNullary temp BoolType addr1
    int2 <- newLabelNum
    let label2 = LabInstr int2
    outWithSuspendedLabel $ TacUnCondJump label2
    labelNext label
    outWithSuspendedLabel $ TacNullary temp BoolType addr2
    labelNext label2
    return . Addr $ temp

checkDim :: Bool -> Integer -> Addr -> Stato ()
checkDim False _ _ = return ()
checkDim True dim a1 = do
    f <- newtemp
    let temp = f BoolType
    int <- newLabelNum
    let label = LabInstr int
    outWithSuspendedLabel $ TacRelCondJump a1 LessThan BoolType (buildTacLiteral . TacLitInt $ dim) label
    outWithSuspendedLabel $ TacCall Nothing (FunId (0, 0) "arrayOutOfBoundAcc_procedure" 0)
    labelNext label
