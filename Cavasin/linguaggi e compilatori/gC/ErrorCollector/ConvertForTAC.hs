module ErrorCollector.ConvertForTAC (convertForTAC, inTreeToOut) where

import AST
import Control.Monad (void)
import Utils

type In = TypeCheckerOutput

type Out = ErrorCollectorOutput

overrideLValue e = updateAnn ((ann e){tcSide = LeftValue}) e

overrideRValue e = updateAnn ((ann e){tcSide = RightValue}) e

eType e = tcType (ann e)

-- eSide e = tcSide (ann e)

convertForTAC = cnvBlock

cnvBlock = emapBlock cnvInstruction

cnvInstruction (Assignment pos expr1 op expr2' x) = Assignment pos expr1 op expr2 x
    where
        expr2 = emap cnvRAssignExpr expr2'
cnvInstruction (Expression pos expr@UnaryOp{} poli) = Expression pos expr' poli
    where
        expr' = emap cnvRAssignExpr expr
cnvInstruction (FunctionDecl pos id args declType block' x) =
    FunctionDecl pos id args declType block x
    where
        block = cnvBlock block'
cnvInstruction (NestedBlock pos block' x) = NestedBlock pos block x
    where
        block = cnvBlock block'
cnvInstruction (While pos expr block' x) = While pos expr block x
    where
        block = cnvBlock block'
cnvInstruction (DoWhile pos expr block' x) = DoWhile pos expr block x
    where
        block = cnvBlock block'
cnvInstruction (For pos id fromExpr toExpr incrExpr block' x) = For pos id fromExpr toExpr incrExpr block x
    where
        block = cnvBlock block'
cnvInstruction (IfThen pos expr block' x) = IfThen pos expr block x
    where
        block = cnvBlock block'
cnvInstruction (IfThenElse pos expr block1' block2' x) = IfThenElse pos expr block1 block2 x
    where
        block1 = cnvBlock block1'
        block2 = cnvBlock block2'
cnvInstruction (TryCatch pos block1' block2' x) = TryCatch pos block1 block2 x
    where
        block1 = cnvBlock block1'
        block2 = cnvBlock block2'
cnvInstruction is = is

cnvRAssignExpr :: Expr In -> Expr In
cnvRAssignExpr expr@ArrayAcc{} = case eType expr of
    ArrayType{} -> overrideLValue expr
    _ -> overrideRValue expr
cnvRAssignExpr (UnaryOp pos PreDecr expr@ArrayAcc{} x) = UnaryOp pos PreDecr (overrideLValue expr) x
cnvRAssignExpr (UnaryOp pos PostDecr expr@ArrayAcc{} x) = UnaryOp pos PostDecr (overrideLValue expr) x
cnvRAssignExpr (UnaryOp pos PreIncr expr@ArrayAcc{} x) = UnaryOp pos PreIncr (overrideLValue expr) x
cnvRAssignExpr (UnaryOp pos PostIncr expr@ArrayAcc{} x) = UnaryOp pos PostIncr (overrideLValue expr) x
cnvRAssignExpr (Ref pos expr@ArrayAcc{} x) = Ref pos (overrideLValue expr) x
cnvRAssignExpr (Deref pos expr@ArrayAcc{} x) = Deref pos (overrideLValue expr) x
cnvRAssignExpr expr = expr

-- fmap, only works on annotations (f a -> f b)

inTreeToOut :: Block In -> Block Out
inTreeToOut = (fmap . fmap) f
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
