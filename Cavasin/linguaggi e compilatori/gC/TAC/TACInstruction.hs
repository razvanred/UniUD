{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TAC.TACInstruction where

import AST
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import TAC.TAC
import TAC.TACexpr
import TAC.TACutils

tacProgram :: [Instruction ErrorCollectorOutput] -> Stato ()
tacProgram block = do
    let progvar1 = ProgVar (buildProgVariable "Str1" (0, 0) (PointerType CharType) ModalityVal) $ PointerType CharType
    let progvar2 = ProgVar (buildProgVariable "Str2" (0, 0) (PointerType CharType) ModalityVal) $ PointerType CharType
    setupStringEqualityFunction progvar1 progvar2 >> tacBlock block

setupStringEqualityFunction :: Addr -> Addr -> Stato ()
setupStringEqualityFunction addr1 addr2 = do
    f1 <- newtemp
    let temp1 = f1 CharType
    f2 <- newtemp
    let temp2 = f2 CharType
    int <- newLabelNum
    let labelFun = LabFunId "stringEq" 0 0
    labelNext labelFun
    outWithSuspendedLabel $ TacPointerLoad temp1 CharType addr1
    outWithSuspendedLabel $ TacPointerLoad temp2 CharType addr2
    int <- newLabelNum
    let labelGuard = LabGuard int
    int1 <- newLabelNum
    let labelAfter = LabInstr int1
    outWithSuspendedLabel $ TacUnCondJump labelGuard
    int2 <- newLabelNum
    let labelBody = LabBodyStart int2
    labelNext labelBody
    f3 <- newtemp
    let temp3 = f3 BoolType
    outWithSuspendedLabel $ TacBinary temp3 (RelationalOp Eq) BoolType temp1 (buildTacLiteral . TacLitChar $ '\0')
    int3 <- newLabelNum
    let labelTrue = LabInstr int3
    outWithSuspendedLabel $ TacBoolCondJump True temp3 labelTrue
    outWithSuspendedLabel $ TacPointerBinary addr1 AddrPlusInt (PointerType CharType) addr1 (buildTacLiteral . TacLitInt . sizeof $ CharType)
    outWithSuspendedLabel $ TacPointerBinary addr2 AddrPlusInt (PointerType CharType) addr2 (buildTacLiteral . TacLitInt . sizeof $ CharType)
    outWithSuspendedLabel $ TacPointerLoad temp1 CharType addr1
    outWithSuspendedLabel $ TacPointerLoad temp2 CharType addr2
    f4 <- newtemp
    let temp4 = f4 BoolType
    labelNext labelGuard
    outWithSuspendedLabel $ TacBinary temp4 (RelationalOp Eq) BoolType temp1 temp2
    outWithSuspendedLabel $ TacBoolCondJump True temp4 labelBody
    labelNext labelAfter
    outWithSuspendedLabel $ TacReturn (Just (BoolType, buildTacLiteral . TacLitBool $ False))
    labelNext labelTrue
    outWithSuspendedLabel $ TacReturn (Just (BoolType, buildTacLiteral . TacLitBool $ True))

tacBlock :: [Instruction ErrorCollectorOutput] -> Stato ()
tacBlock [] = return ()
tacBlock (x : xs) = case x of
    throw@(Throw{}) -> tacBlock xs
    _ -> tacInstr x >> tacBlock xs

tacBlockForResParam :: [Instruction ErrorCollectorOutput] -> [Parameter ErrorCollectorOutput] -> [(Modality, Type)] -> Stato ()
tacBlockForResParam [] _ _ = return ()
tacBlockForResParam (x : xs) params modstypes = case x of
    ReturnVoid{} -> postambleResParam params modstypes
    ReturnExp{} -> postambleResParam params modstypes >> tacInstr x >> tacBlockForResParam xs params modstypes
    _ -> tacInstr x >> tacBlockForResParam xs params modstypes

tacInstr :: Instruction ErrorCollectorOutput -> Stato ()
tacInstr (NestedBlock _ block _) = tacBlock block
tacInstr (VariableDecl _ _ _ _ (ErrorCollectorOutput _ _ (Just ModalityRef) _)) = error "variable declaration must have byvalue modality"
tacInstr (VariableDecl _ _ _ _ (ErrorCollectorOutput _ _ Nothing _)) = error "variable declaration must have byvalue modality"
tacInstr (VariableDecl pos s decl expr (ErrorCollectorOutput t _ (Just mod@ModalityVal) _)) = case expr of
    ArrayLiteral{} -> do
        let initialization = flatten (expr : [])
        tacArrInit (ProgVar (buildProgVariable s pos t mod) t) (buildTacLiteral . TacLitInt $ 0) initialization (getPrimitiveTypeArr t)
    _ -> do
        let unusedDecl = DVoidType
        let unusedExpr = StringLiteral (0, 0) "" ()
        let tp = ErrorCollectorOutput t Nothing (Just mod) (Just (VariableDecl pos s unusedDecl unusedExpr ()))
        tacInstr (Assignment pos (Id pos s tp) BasicAssignment expr emptyTCO)

-- assumiamo di avere come ultima istruzione un return
tacInstr (FunctionDecl (line, col) id params _ body (ErrorCollectorOutput (FunctionType mt _) _ _ _)) = do
    currentState@(b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    l <- lift get
    put (b, 0, cs, [], [], [LabFunId id line col], [], strings)
    lift (put (currentState : l))
    preambleValResParam params mt
    if paramsContainsResOrValResMod mt
        then tacBlockForResParam body params mt
        else tacBlock body
    (b, completedInt, completedStrings, completedLabels, completedTAC, suspendedLabels, bclabels, strings) <- get
    case suspendedLabels of
        x : [] -> error "At this point there cannot be suspended labels"
        [] -> do
            xs <- lift get
            case xs of
                [] -> return ()
                (b, ci, cs, lab, tac, suslab, bclabels, previousstrings) : rest -> do
                    lift (put rest)
                    put (b, ci, completedStrings, lab ++ completedLabels, tac ++ completedTAC, suslab, bclabels, strings)
tacInstr (IfThen _ expr block _) = do
    int <- newLabelNum
    let label = LabIfFalse int
    tacBoolExpr expr FALL label
    tacBlock block
    labelNext label
tacInstr (IfThenElse _ expr block1 block2 _) = do
    int <- newLabelNum
    let label = LabBodyStart int
    tacBoolExpr expr FALL label
    tacBlock block1
    int2 <- newLabelNum
    let label2 = LabInstr int2
    outWithSuspendedLabel $ TacUnCondJump label2
    labelNext label
    tacBlock block2
    labelNext label2
tacInstr (While _ expr block _) = do
    int <- newLabelNum
    let labelGuard = LabGuard int
    int1 <- newLabelNum
    let labelAfter = LabInstr int1
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    put (b, ci, cs, cl, cTac, sl, (labelGuard, labelAfter) : bclabels, strings)
    outWithSuspendedLabel $ TacUnCondJump labelGuard
    int2 <- newLabelNum
    let labelBody = LabBodyStart int2
    labelNext labelBody
    tacBlock block
    labelNext labelGuard
    tacBoolExpr expr labelBody FALL
    labelNext labelAfter
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "in a while there must be at least a couple of suspended bc labels"
        (x : xs) -> put (b, ci, cs, cl, cTac, sl, xs, strings)
tacInstr (DoWhile _ expr block _) = do
    int <- newLabelNum
    let labelGuard = LabGuard int
    int1 <- newLabelNum
    let labelAfter = LabInstr int1
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    put (b, ci, cs, cl, cTac, sl, (labelGuard, labelAfter) : bclabels, strings)
    int2 <- newLabelNum
    let labelBody = LabBodyStart int2
    labelNext labelBody
    tacBlock block
    labelNext labelGuard
    tacBoolExpr expr labelBody FALL
    labelNext labelAfter
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "in a while there must be at least a couple of suspended bc labels"
        (x : xs) -> put (b, ci, cs, cl, cTac, sl, xs, strings)
tacInstr (For pos id from step to block _) = do
    let unusedExpr = StringLiteral (0, 0) "" ()
    let tp = ErrorCollectorOutput IntType Nothing (Just ModalityVal) (Just (VariableDecl pos id DIntType unusedExpr ()))
    tacInstr (Assignment pos (Id pos id tp) BasicAssignment from emptyTCO)
    f1 <- newtemp
    let temp1 = f1 IntType
    xaddr1 <- tacExpr step
    case xaddr1 of
        RefAddr a -> do
            xtmp <- buildTempANDIndirectLoad a IntType False
            outWithSuspendedLabel $ TacNullary temp1 IntType (addr xtmp)
        Addr a -> outWithSuspendedLabel $ TacNullary temp1 IntType a
    f2 <- newtemp
    let temp2 = f2 IntType
    xaddr2 <- tacExpr to
    case xaddr2 of
        RefAddr a -> do
            xtmp <- buildTempANDIndirectLoad a IntType False
            outWithSuspendedLabel $ TacNullary temp2 IntType (addr xtmp)
        Addr a -> outWithSuspendedLabel $ TacNullary temp2 IntType a
    index <- tacExpr (Id pos id tp)
    f3 <- newtemp
    let temp3 = f3 IntType
    let exprIncr = TacBinary temp3 (ArithmeticOp Add) IntType (addr index) temp1
    let incr = TacNullary (addr index) IntType temp3
    -- let exprIncr = BinaryOp pos (ArithmeticOp Add) (Id pos id tp) (IntLiteral pos 1 tp) tp
    -- let incr = Assignment pos (Id pos id tp) BasicAssignment exprIncr tp
    tacWhileCustom LessThanEq (addr index) temp2 block [exprIncr, incr]
tacInstr Break{} = do
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "a break must find at least a bclabel"
        (x : xs) -> outWithSuspendedLabel $ TacUnCondJump (snd x)
tacInstr Continue{} = do
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "a continue must find at least a bclabel"
        (x : xs) -> outWithSuspendedLabel $ TacUnCondJump (fst x)
tacInstr ReturnVoid{} = outWithSuspendedLabel $ TacReturn Nothing
tacInstr (ReturnExp _ expr _) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr a) -> outWithSuspendedLabel . TacReturn . Just $ (addrT a, a)
        (RefAddr a) -> do
            xaddr1 <- buildTempANDIndirectLoad a (addrT a) False
            -- outWithSuspendedLabel $ TacReturn . Just $ (addrT . addr $ xaddr1, addr xaddr1)
            outWithSuspendedLabel . TacReturn . Just $ (extractTypeFromExpr expr, addr xaddr1)
tacInstr (Expression _ fc@(FunctionCall _ id exprs (ErrorCollectorOutput t _ _ (Just decl))) _) = do
    case t of
        VoidType -> do
            tacActualParameters exprs (getParamList decl)
            let funid = FunId (getLocFromDecl decl) id (length exprs)
            outWithSuspendedLabel $ TacCall Nothing funid
            return ()
        _ -> tacExpr fc >> return ()
tacInstr (Expression _ expr _) = tacExpr expr >> return ()
tacInstr (Assignment _ expr1 aop expr2 _) = do
    xaddr1 <- tacExpr expr1
    xaddr2 <- tacExpr expr2
    case extractTypeFromExpr expr1 of
        at@(ArrayType{}) -> do
            let sliceSize = sizeof at
            let baseType = getPrimitiveTypeArr at
            case (xaddr1, xaddr2) of
                (Addr addr1, Addr addr2) -> do
                    let arr1 = ArrayAddr addr1 (buildTacLiteral . TacLitInt $ 0)
                    let arr2 = ArrayAddr addr2 (buildTacLiteral . TacLitInt $ 0)
                    sliceCopyBasic arr1 baseType sliceSize arr2 aop
                (RefAddr addr1, Addr addr2) -> do
                    let arr2 = ArrayAddr addr2 (buildTacLiteral . TacLitInt $ 0)
                    sliceCopyArrayToPointer addr1 arr2 baseType sliceSize aop
                (Addr addr1, RefAddr addr2) -> do
                    let arr1 = ArrayAddr addr1 (buildTacLiteral . TacLitInt $ 0)
                    sliceCopyPointerToArray arr1 addr2 baseType sliceSize aop
                (RefAddr addr1, RefAddr addr2) ->
                    sliceCopyArrayToArray addr1 addr2 baseType sliceSize aop
                (ArrayAddr b o, Addr addr2) -> do
                    let arr2 = ArrayAddr addr2 (buildTacLiteral . TacLitInt $ 0)
                    sliceCopyBasic xaddr1 baseType sliceSize arr2 aop
                (Addr addr1, ArrayAddr b o) -> do
                    let arr1 = ArrayAddr addr1 (buildTacLiteral . TacLitInt $ 0)
                    sliceCopyBasic arr1 baseType sliceSize xaddr2 aop
                (ArrayAddr b1 o1, ArrayAddr b2 o2) ->
                    sliceCopyBasic xaddr1 baseType sliceSize xaddr2 aop
                (ArrayAddr b o, RefAddr addr2) ->
                    sliceCopyPointerToArray xaddr1 addr2 baseType sliceSize aop
                (RefAddr addr1, ArrayAddr b o) ->
                    sliceCopyArrayToPointer addr1 xaddr2 baseType sliceSize aop
        t -> assignTypeDiffFromArr xaddr1 xaddr2 t aop >> return ()
tacInstr (TryCatch pos block1 block2 _) = do
    int <- newLabelNum
    let labelException = LabGuard int
    tacTCBlock block1 labelException
    int <- newLabelNum
    let labelAfterCatch = LabInstr int
    outWithSuspendedLabel $ TacUnCondJump labelAfterCatch
    labelNext labelException
    tacBlock block2
    labelNext labelAfterCatch
tacInstr (Throw _ str _) = do
    (flag, k, j, labels, tac, suspendedLabels, bcl, strings) <- get
    put (True, k, j, labels, tac, suspendedLabels, bcl, strings)
    outWithSuspendedLabel $ TacThrowException str

tacTCBlock :: [Instruction ErrorCollectorOutput] -> Label -> Stato ()
tacTCBlock [] _ = return ()
tacTCBlock (x : xs) label = case x of
    throw@(Throw{}) -> do
        tacInstr throw
        outWithSuspendedLabel $ TacUnCondJump label
    _ -> do
        tacInstr x
        (flag, k, j, labels, tac, suspendedLabels, bcl, strings) <- get
        when flag $ do
            outWithSuspendedLabel $ TacUnCondJump label
            put (False, k, j, labels, tac, suspendedLabels, bcl, strings)
            tacTCBlock xs label

sliceCopyBasic :: XAddr -> Type -> Integer -> XAddr -> AssignmentOp -> Stato ()
sliceCopyBasic _ _ 0 _ _ = return ()
sliceCopyBasic a1@(ArrayAddr b1 o1) baseType sliceSize a2@(ArrayAddr b2 o2) aop = do
    assignTypeDiffFromArr a1 a2 baseType aop
    case (o1, o2) of
        (TacLit _ _, TacLit _ _) -> do
            let contentOffseta1 = contentInt . tacLit $ o1
            let contentOffseta2 = contentInt . tacLit $ o2
            let newoffseta1 = buildTacLiteral . TacLitInt . (+) contentOffseta1 . sizeof $ baseType
            let newoffseta2 = buildTacLiteral . TacLitInt . (+) contentOffseta2 . sizeof $ baseType
            sliceCopyBasic (ArrayAddr b1 newoffseta1) baseType (sliceSize - sizeof baseType) (ArrayAddr b2 newoffseta2) aop
        (TacLit _ _, _) -> do
            let contentOffseta1 = contentInt . tacLit $ o1
            let newoffseta1 = buildTacLiteral . TacLitInt . (+) contentOffseta1 . sizeof $ baseType
            newoffseta2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 newoffseta1) baseType (sliceSize - sizeof baseType) (ArrayAddr b2 . addr $ newoffseta2) aop
        (_, TacLit _ _) -> do
            let contentOffseta2 = contentInt . tacLit $ o2
            let newoffseta2 = buildTacLiteral . TacLitInt . (+) contentOffseta2 . sizeof $ baseType
            newoffseta1 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 . addr $ newoffseta1) baseType (sliceSize - sizeof baseType) (ArrayAddr b2 newoffseta2) aop
        _ -> do
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 . addr $ xtmp1) baseType (sliceSize - sizeof baseType) (ArrayAddr b2 . addr $ xtmp2) aop

sliceCopyArrayToPointer :: Addr -> XAddr -> Type -> Integer -> AssignmentOp -> Stato ()
sliceCopyArrayToPointer _ _ _ 0 _ = return ()
sliceCopyArrayToPointer addr1 a2@(ArrayAddr b2 o2) baseType sliceSize aop = do
    assignTypeDiffFromArr (RefAddr addr1) a2 baseType aop
    when (sliceSize > sizeof baseType) $ do
        xtmp1 <- buildTempANDPointerTacBinary (PointerType baseType) addr1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
        case o2 of
            TacLit _ _ -> do
                let contentOffset = contentInt . tacLit $ o2
                let newOffset = buildTacLiteral . TacLitInt . (+) contentOffset . sizeof $ baseType
                sliceCopyArrayToPointer (addr xtmp1) (ArrayAddr b2 newOffset) baseType (sliceSize - (sizeof baseType)) aop
            _ -> do
                xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
                sliceCopyArrayToPointer (addr xtmp1) (ArrayAddr b2 . addr $ xtmp2) baseType (sliceSize - (sizeof baseType)) aop

sliceCopyPointerToArray :: XAddr -> Addr -> Type -> Integer -> AssignmentOp -> Stato ()
sliceCopyPointerToArray _ _ _ 0 _ = return ()
sliceCopyPointerToArray a1@(ArrayAddr b1 o1) addr2 baseType sliceSize aop = do
    assignTypeDiffFromArr a1 (RefAddr addr2) baseType aop
    when (sliceSize > sizeof baseType) $ do
        xtmp1 <- buildTempANDPointerTacBinary (PointerType baseType) addr2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
        case o1 of
            TacLit _ _ -> do
                let contentOffset = contentInt . tacLit $ o1
                let newOffset = buildTacLiteral . TacLitInt . (+) contentOffset . sizeof $ baseType
                sliceCopyPointerToArray (ArrayAddr b1 newOffset) (addr xtmp1) baseType (sliceSize - sizeof baseType) aop
            _ -> do
                xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
                sliceCopyPointerToArray (ArrayAddr b1 . addr $ xtmp2) (addr xtmp1) baseType (sliceSize - sizeof baseType) aop

sliceCopyArrayToArray :: Addr -> Addr -> Type -> Integer -> AssignmentOp -> Stato ()
sliceCopyArrayToArray _ _ _ 0 _ = return ()
sliceCopyArrayToArray a1 a2 baseType sliceSize aop = do
    let t = PointerType baseType
    assignTypeDiffFromArr (RefAddr a1) (RefAddr a2) baseType aop
    when (sliceSize > sizeof baseType) $ do
        xtmp1 <- buildTempANDPointerTacBinary t a1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
        xtmp2 <- buildTempANDPointerTacBinary t a2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
        sliceCopyArrayToArray (addr xtmp1) (addr xtmp2) baseType (sliceSize - sizeof baseType) aop

assignTypeDiffFromArr :: XAddr -> XAddr -> Type -> AssignmentOp -> Stato ()
assignTypeDiffFromArr xaddr1 xaddr2 t BasicAssignment = do
    case (xaddr1, xaddr2) of
        (Addr addr1, Addr addr2) -> do
            outWithSuspendedLabel $ TacNullary addr1 t addr2
        (RefAddr addr1, Addr addr2) -> do
            outWithSuspendedLabel $ TacPointerStore addr1 t addr2
        (Addr addr1, RefAddr addr2) -> do
            outWithSuspendedLabel $ TacPointerLoad addr1 t addr2
        (RefAddr addr1, RefAddr addr2) -> do
            temp <- buildTempANDIndirectLoad addr2 t False
            let extractAddrFromTemp = addr temp
            outWithSuspendedLabel $ TacPointerStore addr1 t extractAddrFromTemp
        (ArrayAddr b o, Addr addr2) -> do
            outWithSuspendedLabel $ TacIndexedStore xaddr1 t addr2
        (ArrayAddr b o, RefAddr addr2) -> do
            xtmp <- buildTempANDIndirectLoad addr2 t False
            outWithSuspendedLabel $ TacIndexedStore xaddr1 t (addr xtmp)
        (a1@(ArrayAddr b1 o1), a2@(ArrayAddr b2 o2)) -> do
            xtmp <- buildTempANDTacIndexedLoad t a2
            outWithSuspendedLabel $ TacIndexedStore a1 t (addr xtmp)
        (RefAddr a, a2@(ArrayAddr b1 o1)) -> do
            xtmp <- buildTempANDTacIndexedLoad t a2
            outWithSuspendedLabel $ TacPointerStore a t (addr xtmp)
assignTypeDiffFromArr xaddr1 xaddr2 t aop = do
    case (xaddr1, xaddr2) of
        (Addr addr1, Addr addr2) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacNullary temp t addr1
            outWithSuspendedLabel $ TacBinary addr1 (getOperator aop) t temp addr2
        (RefAddr addr1, Addr addr2) -> do
            xtmp <- buildTempANDIndirectLoad addr1 t False
            xtmp1 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) addr2
            outWithSuspendedLabel $ TacPointerStore addr1 t (addr xtmp1)
        (Addr addr1, RefAddr addr2) -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacNullary temp t addr1
            xtmp1 <- buildTempANDIndirectLoad addr2 t False
            outWithSuspendedLabel $ TacBinary addr1 (getOperator aop) t temp (addr xtmp1)
        (RefAddr addr1, RefAddr addr2) -> do
            xtmp <- buildTempANDIndirectLoad addr1 t False
            xtmp1 <- buildTempANDIndirectLoad addr2 t False
            xtmp2 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) (addr xtmp1)
            outWithSuspendedLabel $ TacPointerStore addr1 t (addr xtmp2)
        (ArrayAddr b o, Addr addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr1
            xtmp1 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) addr2
            outWithSuspendedLabel $ TacIndexedStore xaddr1 t (addr xtmp1)
        (ArrayAddr b o, RefAddr addr2) -> do
            xtmp <- buildTempANDTacIndexedLoad t xaddr1
            xtmp1 <- buildTempANDIndirectLoad addr2 t False
            xtmp2 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) (addr xtmp1)
            outWithSuspendedLabel $ TacIndexedStore xaddr1 t (addr xtmp2)
        (a1@(ArrayAddr b1 o1), a2@(ArrayAddr b2 o2)) -> do
            xtmp <- buildTempANDTacIndexedLoad t a1
            xtmp1 <- buildTempANDTacIndexedLoad t a2
            xtmp2 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) (addr xtmp1)
            outWithSuspendedLabel $ TacIndexedStore a1 t (addr xtmp2)
        (RefAddr a, a2@(ArrayAddr b1 o1)) -> do
            xtmp <- buildTempANDIndirectLoad a t False
            xtmp1 <- buildTempANDTacIndexedLoad t a2
            xtmp2 <- buildTempANDTacBinary (getOperator aop) t (addr xtmp) (addr xtmp1)
            outWithSuspendedLabel $ TacPointerStore a t (addr xtmp2)

postambleResParam :: [Parameter ErrorCollectorOutput] -> [(Modality, Type)] -> Stato ()
postambleResParam [] [] = return ()
postambleResParam ((Param pos _ id _ _) : ps) ((mod, ty) : modtys) = case mod of
    ModalityVal -> postambleResParam ps modtys
    ModalityRef -> postambleResParam ps modtys
    _ -> do
        let unusedDecl = DVoidType
        let unusedExpr = StringLiteral (0, 0) "" ()
        let localId = Id pos id (ErrorCollectorOutput ty Nothing (Just mod) (Just (VariableDecl pos "" unusedDecl unusedExpr ())))
        let formalParam = Id pos id (ErrorCollectorOutput ty Nothing (Just ModalityRef) (Just (VariableDecl pos "" unusedDecl unusedExpr ())))
        tacInstr (Assignment (0, 0) formalParam BasicAssignment localId emptyTCO)
        postambleResParam ps modtys

preambleValResParam :: [Parameter ErrorCollectorOutput] -> [(Modality, Type)] -> Stato ()
preambleValResParam [] [] = return ()
preambleValResParam ((Param pos _ id _ _) : ps) ((mod, ty) : modtys) = case mod of
    ModalityValRes -> do
        let unusedDecl = DVoidType
        let unusedExpr = StringLiteral (0, 0) "" ()
        let localId = Id pos id (ErrorCollectorOutput ty Nothing (Just mod) (Just (VariableDecl pos "" unusedDecl unusedExpr ())))
        let formalParam = Id pos id (ErrorCollectorOutput ty Nothing (Just ModalityRef) (Just (VariableDecl pos "" unusedDecl unusedExpr ())))
        tacInstr (Assignment (0, 0) localId BasicAssignment formalParam emptyTCO)
    _ -> preambleValResParam ps modtys

tacWhileCustom :: RelOp -> Addr -> Addr -> Block ErrorCollectorOutput -> [TAC] -> Stato ()
tacWhileCustom relop addr1 addr2 block lastInstrs = do
    int <- newLabelNum
    let labelGuard = LabGuard int
    int1 <- newLabelNum
    let labelAfter = LabInstr int1
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    put (b, ci, cs, cl, cTac, sl, (labelGuard, labelAfter) : bclabels, strings)
    outWithSuspendedLabel $ TacUnCondJump labelGuard
    int2 <- newLabelNum
    let labelBody = LabBodyStart int2
    labelNext labelBody
    tacBlock block
    mapM_ outWithSuspendedLabel lastInstrs
    labelNext labelGuard
    outWithSuspendedLabel $ TacRelCondJump addr1 relop BoolType addr2 labelBody
    labelNext labelAfter
    (b, ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "in a while there must be at least a couple of suspended bc labels"
        (x : xs) -> put (b, ci, cs, cl, cTac, sl, xs, strings)
