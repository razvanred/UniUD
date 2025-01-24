module TACEmitter.TACgenerator where

import TACEmitter.TAC
import AST
import Control.Monad.Trans.State

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
int2AddrTempName  =  Temporary  

genCode :: MyMon a -> [ TAC ]
genCode gen = reverse $ snd $ execState gen (0 ,[])



--extractType addr = case addr of
--    ProgVar _ t -> t
--    TacLit _ t -> t
--    Temporary _ t -> t


tacExpr :: Expr ASTData -> MyMon XAddr
tacExpr (UnaryOp pos uop expr (TypeChecker t lr _ _) ) =  do
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
         --(ArraAddr base offset ) -> TODO


--expr should have leftvalue information otherwise the semantic static inference is wrong
tacExpr (Deref pos expr (TypeChecker t lr _ _ )) = do
    xaddr <- tacExpr expr
    case (xaddr,lr) of
        (Addr a,Just LeftValue) -> return $ RefAddr a
        (RefAddr a, Just LeftValue) -> return $ RefAddr a
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


--here lr should be lefvalue otherwise the semantic static inference is wrong
tacExpr (Ref pos expr (TypeChecker t lr _ _ )) = do
    xaddr <- tacExpr expr
    case (xaddr) of
        (Addr a) -> return $ RefAddr a
        (RefAddr a) -> return $ RefAddr a


tacExpr (Id pos (Ident ident) (TypeChecker t lr m _ ) ) = case m of
       (Just ModalityVal) -> return . Addr $ ProgVar{progVar= buildProgVariable ident pos t ModalityVal ,addrT = t}
       (Just ModalityRef) -> return . RefAddr $ ProgVar{progVar= buildProgVariable ident pos t ModalityRef ,addrT = t}

tacExpr(BasicLiteral pos bs (TypeChecker t lr m _))  = case bs of 
    IntLiteral i (TypeChecker _ _ _ _) -> return . Addr $ TacLit{tacLit= TacLitInt i, addrT=t}     
    CharLiteral c (TypeChecker _ _ _ _) -> return . Addr $ TacLit{tacLit= TacLitChar c, addrT=t}
    FloatLiteral f (TypeChecker _ _ _ _) -> return . Addr $ TacLit{tacLit= TacLitFloat f, addrT=t}
    BoolLiteral b (TypeChecker _ _ _ _) -> return . Addr $ TacLit{tacLit= TacLitBool b, addrT=t}
    --StringLiteral str (TypeChecker _ _ _ _) ->  Here I give university up!!!!!!!!
          
                 
  
buildProgVariable :: String -> Position -> Type -> Modality -> TacProgVariable
buildProgVariable ident pos t modality = TacProgVar{varName=VarId{vLoc=pos,vId=ident},varModality=modality, varType=t}  


