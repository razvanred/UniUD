{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module TAC.TACutils where

import AST
import Control.Monad.Trans.State
import Data.List (sort)
import Data.Map.Strict qualified as Map
import TAC.TAC
import Utils hiding (error)

type In = ErrorCollectorOutput

emptyTCO = ErrorCollectorOutput VoidType Nothing Nothing Nothing

type Stringhe = Map.Map String String

newtype CoppiaStringhe = CoppiaStringhe (String, String)

instance Eq CoppiaStringhe where
    (==) (CoppiaStringhe (i, j)) (CoppiaStringhe (p, q)) = j == q

instance Ord CoppiaStringhe where
    (<=) (CoppiaStringhe (i, j)) (CoppiaStringhe (p, q)) = j <= q

type AuxStack = State [(Bool, Int, Int, [Label], [TAC], [Label], [(Label, Label)], Stringhe)]

type Stato = StateT (Bool, Int, Int, [Label], [TAC], [Label], [(Label, Label)], Stringhe) AuxStack

-- bool per gestione throw
-- intero per temporanei/label,
-- intero per stringhe statiche
-- listalabel a rovescio
-- listatac a rovescio
-- "pila" di label sospese, impilate per successivo uso
-- "pila" di label per break/continue
-- mappa con valori stringhe e label identificative

checkIfSuspendedLabel :: Stato Bool
checkIfSuspendedLabel = do
    (b, k, j, labels, tac, suspendedLabels, bclabels, strings) <- get
    return . not . null $ suspendedLabels

outWithSuspendedLabel :: TAC -> Stato ()
outWithSuspendedLabel tacInstr = do
    (b, k, j, labels, tac, sl, bcl, strings) <- get
    case sl of
        [] -> put (b, k, j, LabEmpty : labels, tacInstr : tac, sl, bcl, strings)
        (x : xs) -> put (b, k, j, x : labels, tacInstr : tac, xs, bcl, strings)

labelNext :: Label -> Stato ()
labelNext l = do
    (b, k, j, labels, tac, suspendedLabels, bcl, strings) <- get
    put (b, k, j, labels, tac, l : suspendedLabels, bcl, strings)

newtemp :: Stato (Type -> Addr)
newtemp = do
    (b, k, j, labels, tac, suspendedLabels, bcl, strings) <- get
    put (b, k + 1, j, labels, tac, suspendedLabels, bcl, strings)
    return $ int2AddrTempName k

newLabelNum :: Stato Int
newLabelNum = do
    (b, k, j, labels, tac, suspendedLabels, bcl, strings) <- get
    put (b, k + 1, j, labels, tac, suspendedLabels, bcl, strings)
    return k

int2AddrTempName :: Int -> Type -> Addr
int2AddrTempName = Temporary

genCode :: Stato a -> (Bool, Int, Int, [Label], [TAC], [Label], [(Label, Label)], Stringhe)
genCode gen = evalState (execStateT gen (False, 0, 0, [], [], [], [], Map.empty)) []

printOutput :: Stato a -> IO ()
printOutput x = do
    let (_, _, _, labels, tac, _, _, strings) = genCode x
    let orderedLabelsToPrint = map pp . reverse $ labels
    let orderedTACToPrint = map pp . reverse $ tac
    let listStrings = Map.toList strings
    let orderedStrings = sort listStrings
    let maxpadding = maximum . map length $ orderedLabelsToPrint
    let paddedLabels = map (\x -> x ++ replicate (maxpadding - length x) ' ') orderedLabelsToPrint
    putStrLn "### STATIC DATA ###"
    mapM_ (\(x, y) -> putStrLn (y ++ " " ++ x)) orderedStrings
    putStrLn "### PROGRAM ###"
    let out = zip paddedLabels orderedTACToPrint
    mapM_ (\(x, y) -> putStrLn (x ++ " " ++ y)) out

printProgram :: [Label] -> [TAC] -> Stringhe -> IO ()
printProgram orderedLabels orderedTac strings = do
    putStrLn "### STATIC DATA ###"
    let listStrings = Map.toList strings
    let orderedStrings = sort listStrings
    mapM_ (\(x, y) -> putStrLn (y ++ ":" ++ x)) orderedStrings
    putStrLn "### PROGRAM ###"
    let out = zip orderedLabels orderedTac
    mapM_ (\(x, y) -> putStrLn (pp x ++ ":" ++ pp y)) out

buildTempANDIndirectLoad :: Addr -> Type -> Bool -> Stato XAddr
buildTempANDIndirectLoad addr t wannaRefAddr = do
    case t of
        (PointerType p) -> do
            f <- newtemp
            let temp = f p
            outWithSuspendedLabel $ TacPointerLoad temp t addr
            case wannaRefAddr of
                True -> return $ RefAddr temp
                False -> return $ Addr temp
        _ -> do
            f <- newtemp
            let temp = f t
            outWithSuspendedLabel $ TacPointerLoad temp t addr
            case wannaRefAddr of
                True -> return $ RefAddr temp
                False -> return $ Addr temp

buildTempANDTacBinary :: BinaryOp -> Type -> Addr -> Addr -> Stato XAddr
buildTempANDTacBinary bop t addr1 addr2 = do
    f <- newtemp
    let temp = f t
    outWithSuspendedLabel $ TacBinary temp bop t addr1 addr2
    return $ Addr temp

buildTempANDPointerTacBinary :: Type -> Addr -> Addr -> Stato XAddr
buildTempANDPointerTacBinary t addr1 addr2 = do
    f <- newtemp
    let temp = f t
    outWithSuspendedLabel $ TacPointerBinary temp AddrPlusInt t addr1 addr2
    return $ Addr temp

buildTempANDTacIndexedLoad :: Type -> XAddr -> Stato XAddr
buildTempANDTacIndexedLoad t addr = do
    f <- newtemp
    let temp = f t
    outWithSuspendedLabel $ TacIndexedLoad temp t addr
    return $ Addr temp

buildProgVariable :: String -> Position -> Type -> Modality -> TacProgVariable
buildProgVariable ident pos t modality = TacProgVar{varName = VarId{vLoc = pos, vId = ident}, varModality = modality, varType = t}

buildTacLiteral :: TacLit -> Addr
buildTacLiteral tacLit = case tacLit of
    int@(TacLitInt _) -> TacLit{tacLit = int, addrT = IntType}
    float@(TacLitFloat _) -> TacLit{tacLit = float, addrT = FloatType}
    char@(TacLitChar _) -> TacLit{tacLit = char, addrT = CharType}
    bool@(TacLitBool _) -> TacLit{tacLit = bool, addrT = BoolType}

buildTacLiteralOneType :: Type -> Addr
buildTacLiteralOneType t = case t of
    IntType -> buildTacLiteral . TacLitInt $ 1
    FloatType -> buildTacLiteral . TacLitFloat $ 1.0
    CharType -> buildTacLiteral . TacLitChar $ '1'
    x -> error ("Cannot build a TacLiteral for type " ++ show x)

sizeof :: Type -> Integer
sizeof x = case x of
    BoolType -> 1
    CharType -> 1
    IntType -> 4
    PointerType _ -> 8
    FloatType -> 8
    ArrayType _ i t -> i * sizeof t
    StringType -> 8
    _ -> error "Cannot be another type"

flatten :: [Expr a] -> [Expr a]
flatten listExpr = reverse $ flatten' listExpr []
    where
        flatten' [] acc = acc
        flatten' (x : xs) acc = case x of
            (ArrayLiteral _ (y : ys) _) -> flatten' xs (flatten' ys (flatten' (y : []) acc))
            _ -> flatten' xs (x : acc)

getPrimitiveTypeArr :: Type -> Type
getPrimitiveTypeArr (ArrayType _ _ t) = getPrimitiveTypeArr t
getPrimitiveTypeArr x = case x of
    bool@BoolType -> bool
    char@CharType -> char
    int@IntType -> int
    float@FloatType -> float
    PointerType t -> PointerType t
    StringType -> PointerType CharType
    _ -> error "array element cannot be of that type"

getOperator :: AssignmentOp -> BinaryOp
getOperator x = case x of
    BasicAssignment -> error "no case here"
    AssignMul -> ArithmeticOp Mul
    AssignAdd -> ArithmeticOp Add
    AssignDiv -> ArithmeticOp Div
    AssignSub -> ArithmeticOp Sub
    AssignPow -> ArithmeticOp Pow
    AssignAnd -> BooleanOp And
    AssignOr -> BooleanOp Or

extractTypeFromExpr :: Expr ErrorCollectorOutput -> Type
extractTypeFromExpr x = case x of
    (UnaryOp _ _ _ tco) -> t tco
    (BinaryOp _ _ _ _ tco) -> t tco
    (Ref _ _ tco) -> t tco
    (Deref _ _ tco) -> t tco
    (ArrayAcc _ _ _ tco) -> t tco
    (Id _ _ tco) -> t tco
    FunctionCall _ _ _ tco -> t tco
    (IntLiteral _ _ tco) -> t tco
    (CharLiteral _ _ tco) -> t tco
    (StringLiteral _ _ tco) -> t tco
    (FloatLiteral _ _ tco) -> t tco
    (BoolLiteral _ _ tco) -> t tco
    ArrayLiteral _ _ tco -> t tco

oppositeRel :: RelOp -> RelOp
oppositeRel relOp = case relOp of
    NotEq -> Eq
    Eq -> NotEq
    GreaterThanEq -> LessThan
    LessThan -> GreaterThanEq
    GreaterThan -> LessThanEq
    LessThanEq -> GreaterThan

getLocFromDecl :: Instruction () -> Position
getLocFromDecl x = case x of
    (VariableDecl pos _ _ _ _) -> pos
    (FunctionDecl pos _ _ _ _ _) -> pos
    _ -> error "This function should not be used on this type."

getParamList :: Instruction () -> [Parameter ()]
getParamList x = case x of
    (FunctionDecl _ _ paramList _ _ _) -> paramList
    _ -> error "This function should not be used on this type."

paramsContainsResOrValResMod :: [(Modality, Type)] -> Bool
paramsContainsResOrValResMod [] = False
paramsContainsResOrValResMod ((mod, ty) : modtys) = case mod of
    ModalityRes -> True
    ModalityValRes -> True
    _ -> paramsContainsResOrValResMod modtys
