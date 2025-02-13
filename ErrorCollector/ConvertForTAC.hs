module ErrorCollector.ConvertForTAC where

import AST
import Control.Monad (void)
import Utils

type In = TypeCheckerOutput

type Out = ErrorCollectorOutput

overrideLValue e = updateAnn ((ann e){tcSide = LeftValue}) e

-- tree traversa, access to nodes

cnvTree = astEmap cnvInstruction cnvDeclType cnvExpr

cnvInstruction :: Instruction In -> Instruction In
cnvInstruction = idty

cnvDeclType :: DeclType In -> DeclType In
cnvDeclType = idty

cnvExpr :: Expr In -> Expr In
cnvExpr expr@(ArrayAcc _ _ _ TypeCheckerOutput{tcSide = RightValue}) = overrideLValue expr
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
