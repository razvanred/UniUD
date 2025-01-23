module TACEmitter.TACgenerator where

import TACEmitter.TAC
import AST
import Control.Monad.Trans.State

type MyMon = State (Int, [TAC])

out :: TAC -> MyMon ()
out instr = do
    (k, revcode) <- get
    put (k, instr : revcode)

newtemp :: MyMon (Type -> XAddr)
newtemp = do
    (k, revcode) <- get
    put (k + 1, revcode)
    return $ int2AddrTempName k

int2AddrTempName :: Int -> (Type -> XAddr)
int2AddrTempName k = Addr . Temporary k 

genCode :: MyMon a -> [ TAC ]
genCode gen = reverse $ snd $ execState gen (0 ,[])

tacExpr :: Expr ASTData -> MyMon XAddr
tacExpr x = case x of
    UnaryOp pos uop expr (TypeChecker t lr instr) -> do
        f <- newtemp
        let temp = f t
        addr <- tacExpr expr
        out $ TacUnary temp uop t addr
        return temp


