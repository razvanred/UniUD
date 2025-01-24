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
tacExpr (UnaryOp pos uop expr (TypeChecker t lr  m _) ) =  do
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


buildProgVariable id pos type modality = TacProgVar{varName=VarId{vLoc=pos,vId=id},varModality=modality, varType=type}  

--tacExpr (Id pos id (TypeChecker t lr m _ ) ) = case m of
--       (ModalityVal) -> return $ ProgVar{progVar= TacProgVar{varName= VarId{vLoc=pos,vId=id},varModality=ModalityRef, varType=t} ,addrT = t}
--       (ModalityRef) -> 

