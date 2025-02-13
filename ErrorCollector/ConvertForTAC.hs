module ErrorCollector.ConvertForTAC where

import AST hiding (astEmap)
import Control.Monad (void)
-- import TypeChecker.TypeUtils hiding (In, Out)
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
cnvInstruction is = is

-- cnvDeclType :: DeclType In -> DeclType In
-- cnvDeclType = idty

cnvRAssignExpr :: Expr In -> Expr In
cnvRAssignExpr expr@ArrayAcc{} = case eType expr of
    ArrayType{} -> overrideLValue expr
    _ -> overrideRValue expr
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
