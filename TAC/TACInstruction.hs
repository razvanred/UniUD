{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module TAC.TACInstruction where

import AST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import TAC.TAC
import TAC.TACexpr
import TAC.TACutils

tacBlock :: [Instruction ErrorCollectorOutput] -> Stato ()
tacBlock [] = return ()
tacBlock (x : xs) = tacInstr x >> tacBlock xs

tacInstr :: Instruction ErrorCollectorOutput -> Stato ()
tacInstr (NestedBlock _ block _) = tacBlock block
tacInstr (VariableDecl _ _ _ _ (ErrorCollectorOutput _ _ (Just ModalityRef) _)) = error "variable declaration must have byvalue modality"
tacInstr (VariableDecl _ _ _ _ (ErrorCollectorOutput _ _ Nothing _)) = error "variable declaration must have byvalue modality"
tacInstr (VariableDecl pos s decl expr tp@(ErrorCollectorOutput t _ (Just mod@ModalityVal) _)) = case expr of
    ArrayLiteral _ _ _ -> do
        let initialization = flatten (expr : [])
        tacArrInit (ProgVar (buildProgVariable s pos t mod) t) (buildTacLiteral . TacLitInt $ 0) initialization (getPrimitiveTypeArr t)
    _ -> do
        tacInstr (Assignment pos (Id pos s tp) BasicAssignment expr emptyTCO)
tacInstr (FunctionDecl (line, col) id _ _ body _) = do
    currentState@(ci, cs, cl, cTac, sl, bclabels, strings) <- get
    l <- lift get
    put (0, cs, [], [], [LabFunId id line col], [], strings)
    lift (put (currentState : l))
    tacBlock body
    (completedInt, completedStrings, completedLabels, completedTAC, suspendedLabels, bclabels, strings) <- get
    case suspendedLabels of
        x : [] -> error "At this point there cannot be suspended labels"
        [] -> do
            xs <- lift get
            case xs of
                [] -> return ()
                (ci, cs, lab, tac, suslab, bclabels, previousstrings) : rest -> do
                    lift (put rest)
                    put (ci, completedStrings, lab ++ completedLabels, tac ++ completedTAC, suslab, bclabels, strings)
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
    (ci, cs, cl, cTac, sl, bclabels, strings) <- get
    put (ci, cs, cl, cTac, sl, (labelGuard, labelAfter) : bclabels, strings)
    outWithSuspendedLabel $ TacUnCondJump labelGuard
    int2 <- newLabelNum
    let labelBody = LabBodyStart int2
    labelNext labelBody
    tacBlock block
    labelNext labelGuard
    tacBoolExpr expr labelBody FALL
    labelNext labelAfter
    (ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "in a while there must be at least a couple of suspended bc labels"
        (x : xs) -> put (ci, cs, cl, cTac, sl, xs, strings)
tacInstr (Break _ _) = do
    (ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "a break must find at least a bclabel"
        (x : xs) -> outWithSuspendedLabel $ TacUnCondJump (snd x)
tacInstr (Continue _ _) = do
    (ci, cs, cl, cTac, sl, bclabels, strings) <- get
    case bclabels of
        [] -> error "a continue must find at least a bclabel"
        (x : xs) -> outWithSuspendedLabel $ TacUnCondJump (fst x)
tacInstr (ReturnVoid _ _) = outWithSuspendedLabel $ TacReturn Nothing
tacInstr (ReturnExp _ expr _) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr a) -> outWithSuspendedLabel . TacReturn . Just $ (addrT a, a)
        (RefAddr a) -> do
            xaddr1 <- buildTempANDIndirectLoad a (addrT a) False
            outWithSuspendedLabel $ TacReturn . Just $ (addrT . addr $ xaddr1, addr xaddr1)
tacInstr (Expression _ fc@(FunctionCall _ id exprs (ErrorCollectorOutput t _ _ (Just decl))) _) = do
    tacActualParameters exprs (getParamList decl)
    case t of
        VoidType -> do
            let funid = FunId (getLocFromDecl decl) id (length exprs)
            outWithSuspendedLabel $ TacCall Nothing funid
            return ()
        _ -> tacExpr fc >> return ()
tacInstr (Expression _ expr _) = tacExpr expr >> return ()
tacInstr (Assignment _ expr1 aop expr2 _) = do
    xaddr1 <- tacExpr expr1
    xaddr2 <- tacExpr expr2
    case (extractTypeFromExpr expr1) of
        at@(ArrayType _ _) -> do
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
        _ -> assignTypeDiffFromArr xaddr1 xaddr2 (addrT . addr $ xaddr1) aop >> return ()

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
            sliceCopyBasic (ArrayAddr b1 newoffseta1) baseType (sliceSize - (sizeof baseType)) (ArrayAddr b2 newoffseta2) aop
        (TacLit _ _, _) -> do
            let contentOffseta1 = contentInt . tacLit $ o1
            let newoffseta1 = buildTacLiteral . TacLitInt . (+) contentOffseta1 . sizeof $ baseType
            newoffseta2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 newoffseta1) baseType (sliceSize - (sizeof baseType)) (ArrayAddr b2 . addr $ newoffseta2) aop
        (_, TacLit _ _) -> do
            let contentOffseta2 = contentInt . tacLit $ o2
            let newoffseta2 = buildTacLiteral . TacLitInt . (+) contentOffseta2 . sizeof $ baseType
            newoffseta1 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 . addr $ newoffseta1) baseType (sliceSize - (sizeof baseType)) (ArrayAddr b2 newoffseta2) aop
        _ -> do
            xtmp1 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyBasic (ArrayAddr b1 . addr $ xtmp1) baseType (sliceSize - (sizeof baseType)) (ArrayAddr b2 . addr $ xtmp2) aop

sliceCopyArrayToPointer :: Addr -> XAddr -> Type -> Integer -> AssignmentOp -> Stato ()
sliceCopyArrayToPointer _ _ _ 0 _ = return ()
sliceCopyArrayToPointer addr1 a2@(ArrayAddr b2 o2) baseType sliceSize aop = do
    -- xtmp <- buildTempANDTacIndexedLoad baseType a2
    -- outWithSuspendedLabel $ TacPointerStore addr1 baseType temp
    assignTypeDiffFromArr (RefAddr addr1) a2 baseType aop
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
    -- f <- newtemp
    -- let temp = f baseType
    -- outWithSuspendedLabel $ TacPointerLoad temp baseType addr2
    -- outWithSuspendedLabel $ TacIndexedStore a1 baseType temp
    assignTypeDiffFromArr a1 (RefAddr addr2) baseType aop
    xtmp1 <- buildTempANDPointerTacBinary (PointerType baseType) addr2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
    case o1 of
        TacLit _ _ -> do
            let contentOffset = contentInt . tacLit $ o1
            let newOffset = buildTacLiteral . TacLitInt . (+) contentOffset . sizeof $ baseType
            sliceCopyPointerToArray (ArrayAddr b1 newOffset) (addr xtmp1) baseType (sliceSize - (sizeof baseType)) aop
        _ -> do
            xtmp2 <- buildTempANDTacBinary (ArithmeticOp Add) IntType o1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
            sliceCopyPointerToArray (ArrayAddr b1 . addr $ xtmp2) (addr xtmp1) baseType (sliceSize - (sizeof baseType)) aop

sliceCopyArrayToArray :: Addr -> Addr -> Type -> Integer -> AssignmentOp -> Stato ()
sliceCopyArrayToArray _ _ _ 0 _ = return ()
sliceCopyArrayToArray a1 a2 baseType sliceSize aop = do
    let t = PointerType baseType
    -- xtmp <- buildTempANDIndirectLoad a2 baseType False
    -- outWithSuspendedLabel $ TacPointerStore a1 baseType (addr xtmp)
    assignTypeDiffFromArr (RefAddr a1) (RefAddr a2) baseType aop
    xtmp1 <- buildTempANDPointerTacBinary t a1 (buildTacLiteral . TacLitInt . sizeof $ baseType)
    xtmp2 <- buildTempANDPointerTacBinary t a2 (buildTacLiteral . TacLitInt . sizeof $ baseType)
    sliceCopyArrayToArray (addr xtmp1) (addr xtmp2) baseType (sliceSize - (sizeof baseType)) aop

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
