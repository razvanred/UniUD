module TACEmitter.TACgenerator where

import AST
import Control.Monad.Trans.State
import Foreign (new)
import TACEmitter.TAC

type In = TypeCheckerOutput
type MyMon = State (Int, [TAC])

out :: TAC -> MyMon ()
out instr = do
    (k, revcode) <- get
    put (k, instr : revcode)

newtemp :: MyMon (Type -> Addr)
newtemp = do
    (k, revcode) <- get
    put (k + 1, revcode)
    return $ int2AddrTempName k

int2AddrTempName :: Int -> Type -> Addr
int2AddrTempName = Temporary

genCode :: MyMon a -> [TAC]
genCode gen = reverse $ snd $ execState gen (0, [])

extractType :: Addr -> Type
extractType addr = case addr of
    ProgVar _ t -> t
    TacLit _ t -> t
    Temporary _ t -> t

tacExpr :: Expr In -> MyMon XAddr
tacExpr (UnaryOp pos uop expr (TypeCheckerOutput t lr _ )) = do
    f <- newtemp
    let temp = f t
    xaddr <- tacExpr expr
    case xaddr of
        (Addr addr) -> do
            out $ TacUnary temp uop t addr
            return $ Addr temp
        (RefAddr addr) -> do
            f1 <- newtemp
            let temp1 = f1 t
            out $ TacPointerLoad temp1 t addr
            out $ TacUnary temp uop t temp1
            return $ Addr temp
-- (ArraAddr base offset ) -> TODO
tacExpr (BinaryOp pos bop expr1 expr2 (TypeCheckerOutput t lr _ )) = do
    xaddr1 <- tacExpr expr1
    xaddr2 <- tacExpr expr2
    case (xaddr1, xaddr2) of
        (Addr addr1, Addr addr2) -> do
            f <- newtemp
            let temp = f t
            out $ TacBinary temp bop t addr1 addr2
            return $ Addr temp
        (RefAddr addr1, Addr addr2) -> do
            f <- newtemp
            let temp = f t
            out $ TacPointerLoad temp t addr1
            f' <- newtemp
            let temp' = f t
            out $ TacBinary temp' bop t temp addr2
            return $ Addr temp'
        (Addr addr1, RefAddr addr2) -> do
            f <- newtemp
            let temp = f t
            out $ TacPointerLoad temp t addr2
            f' <- newtemp
            let temp' = f t
            out $ TacBinary temp' bop t addr1 temp
            return $ Addr temp'
        (RefAddr addr1, RefAddr addr2) -> do
            f1 <- newtemp
            let temp1 = f1 t
            out $ TacPointerLoad temp1 t addr1
            f2 <- newtemp
            let temp2 = f2 t
            out $ TacPointerLoad temp2 t addr2
            f3 <- newtemp
            let temp3 = f3 t
            out $ TacBinary temp3 bop t temp1 temp2
            return $ Addr temp3

-- expr should have leftvalue information otherwise the semantic static inference is wrong
tacExpr (Deref pos expr (TypeCheckerOutput t lr _ )) = do
    xaddr <- tacExpr expr
    case (xaddr, lr) of
        (Addr a, Just LeftValue) -> do
            return $ RefAddr a
        (RefAddr a, Just LeftValue) -> do
            case extractType a of
                PointerFType t -> do
                    f <- newtemp
                    let temp = f t
                    out $ TacPointerLoad temp t a
                    return $ Addr temp 
        (Addr a, Just RightValue) -> do
            f <- newtemp
            let temp = f t
            out $ TacPointerLoad temp t a
            return $ Addr temp
        (RefAddr a, Just RightValue) -> do
            f <- newtemp
            let temp = f t
            out $ TacNullary temp t a
            return $ Addr temp


tacExpr (Ref pos expr (TypeCheckerOutput t lr _ )) = do
    xaddr <- tacExpr expr
    case xaddr of
        (Addr a) -> do
                    f <- newtemp
                    let temp = f t
                    out $ TacReferenceLoad temp t a
                    return $ Addr temp 
        (RefAddr a) -> return $ RefAddr a

tacExpr (Id pos (Ident ident) (TypeCheckerOutput t lr m )) = case m of
    (Just ModalityVal) -> return . Addr $ ProgVar {progVar = buildProgVariable ident pos t ModalityVal, addrT = t}
    (Just ModalityRef) -> return . RefAddr $ ProgVar {progVar = buildProgVariable ident pos t ModalityRef, addrT = t}

tacExpr (BasicLiteral pos bs (TypeCheckerOutput t lr m )) = case bs of
    IntLiteral i (TypeCheckerOutput _ _ _ ) -> return . Addr $ TacLit {tacLit = TacLitInt i, addrT = t}
    CharLiteral c (TypeCheckerOutput _ _ _ ) -> return . Addr $ TacLit {tacLit = TacLitChar c, addrT = t}
    FloatLiteral f (TypeCheckerOutput _ _ _ ) -> return . Addr $ TacLit {tacLit = TacLitFloat f, addrT = t}
    BoolLiteral b (TypeCheckerOutput _ _ _ ) -> return . Addr $ TacLit {tacLit = TacLitBool b, addrT = t}
-- StringLiteral str (TypeCheckerOutput _ _ _ _) ->  Here I give university up!!!!!!!!

buildProgVariable :: String -> Position -> Type -> Modality -> TacProgVariable
buildProgVariable ident pos t modality = TacProgVar {varName = VarId {vLoc = pos, vId = ident}, varModality = modality, varType = t}
