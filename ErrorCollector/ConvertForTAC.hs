module ErrorCollector.ConvertForTAC where

import AST hiding (astEmap)
import Control.Monad (void)
import Utils

type In = TypeCheckerOutput

type Out = ErrorCollectorOutput

overrideLValue e = updateAnn ((ann e){tcSide = LeftValue}) e

overrideRValue e = updateAnn ((ann e){tcSide = RightValue}) e

eType e = tcType (ann e)

eSide e = tcSide (ann e)

cnvBlock = emapBlock cnvInstruction

cnvInstruction :: Instruction In -> Instruction In
cnvInstruction (Assignment pos expr1 op expr2' x) = Assignment pos expr1 op expr2 x
    where
        expr2 = emap cnvRAssignExpr expr2'
cnvInstruction (Expression pos expr@UnaryOp{} poli) = Expression pos expr' poli
    where 
        expr' = emap cnvRAssignExpr expr
cnvInstruction is = is


-- cnvDeclType :: DeclType In -> DeclType In
-- cnvDeclType = idty

cnvRAssignExpr :: Expr In -> Expr In
cnvRAssignExpr expr@ArrayAcc{} = case eType expr of
    ArrayType{} -> overrideLValue expr
    _ -> overrideRValue expr
cnvRAssignExpr (UnaryOp pos PreDecr expr@ArrayAcc{} poli) = UnaryOp pos PreDecr (overrideLValue expr) poli  
cnvRAssignExpr (UnaryOp pos PostDecr expr@ArrayAcc{} poli) = UnaryOp pos PostDecr (overrideLValue expr) poli    
cnvRAssignExpr (UnaryOp pos PreIncr expr@ArrayAcc{} poli) = UnaryOp pos PreIncr (overrideLValue expr) poli    
cnvRAssignExpr (UnaryOp pos PostIncr expr@ArrayAcc{} poli) = UnaryOp pos PostIncr (overrideLValue expr) poli     
cnvRAssignExpr (Ref pos expr@ArrayAcc{} poli) = Ref pos (overrideLValue expr) poli      
cnvRAssignExpr (Deref pos expr@ArrayAcc{} poli) = Deref pos (overrideLValue expr) poli      
cnvRAssignExpr expr = expr

-- fmap, only works on annotations (f a -> f b)

inTreeToOut tree = (fmap . fmap) f tree
    where
        f
            TypeCheckerOutput
                { tcType,
                  tcSide,
                  tcBinding
                } = ErrorCollectorOutput tcType (Just tcSide) (getModty tcBinding) (getBind tcBinding)
        getModty binding
            | Just (_, modty, _) <- binding = Just modty
            | otherwise = Just ModalityVal
        getBind binding
            | Just (_, _, is) <- binding = Just (void is)
            | otherwise = Nothing
