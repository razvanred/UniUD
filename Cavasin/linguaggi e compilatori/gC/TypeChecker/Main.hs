module TypeChecker.Main (staticAnalizer) where

import AST
import Control.Monad (void)
import TypeChecker.Checker
import TypeChecker.ConstExprSolver
import TypeChecker.Totality
import TypeChecker.TypeUtils
import Utils

type In = ConstantSolverOutput

type Out = TypeCheckerOutput

staticAnalizer :: Block In -> Block Out
staticAnalizer block = out
    where
        step1 = (fmap . fmap) inToStep1 block
        step2 = solveConstExpr step1
        step3 = checkTypes step2
        step3pass2 = checkTotality step3
        out = (fmap . fmap) step3ToOut step3pass2

inToStep1 x = Step1 (cserrors x) (cswarnings x) (csReplacedFromConstant x)

step3ToOut x@Step3{} =
    TypeCheckerOutput
        { tcerrors = serrors x,
          tcwarnings = swarnings x,
          tcReplacedFromConstant = sReplacedFromConstant x,
          tcType = sType x,
          tcSide = sSide x,
          tcBinding = f <$> sBinding x
        }
    where
        f (depth, modty, is) = (depth, modty, void is)
step3ToOut x = "record " ++ show x `unexpectedDuring` "stepnToOut"
