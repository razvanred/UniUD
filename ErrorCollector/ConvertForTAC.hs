module ErrorCollector.ConvertForTAC where

import AST
import Control.Monad (void)
-- import TypeChecker.TypeUtils hiding (In, Out)
import Utils

type In = TypeCheckerOutput

type Out = ErrorCollectorOutput

overrideLValue e = updateAnn ((ann e){tcSide = LeftValue}) e

overrideRValue e = updateAnn ((ann e){tcSide = RightValue}) e

-- tree traversa, access to nodes

eType e = tcType (ann e)

eSide e = tcSide (ann e)

cnvBlock = astEmap cnvInstruction idty cnvDeclType cnvExpr

cnvInstruction :: Instruction In -> Instruction In
cnvInstruction is = is

-- cnvInstruction (Assignment pos expr1 op expr2' x)
--     | ArrayAcc{} <- expr2',
--       ArrayType{} <- eType expr2' =
--         Assignment pos expr1 op expr2 x
--     where
--         expr2 = updateAnn ((ann expr2){tcSide = LeftValue}) expr2

cnvDeclType :: DeclType In -> DeclType In
cnvDeclType = idty

cnvExpr :: Expr In -> Expr In
cnvExpr expr@(ArrayAcc _ _ _ TypeCheckerOutput{tcType = ArrayType{}}) = overrideLValue expr
cnvExpr expr@(ArrayAcc _ _ _ TypeCheckerOutput{}) = overrideRValue expr
cnvExpr expr = expr

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
