-- {-# OPTIONS_GHC -w #-}

module ITest where

import ErrorCollector.ConvertForTAC (cnvTree, inTreeToOut)
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TypeChecker.Algs qualified as CS2
import TypeChecker.Checker qualified as Checker
import TypeChecker.ConstExprSolver qualified as Checker
import TypeChecker.TypeUtils (inToStep1, stepnToOut)
import Utils (prp)

f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildTree pt
    let t1 = CS2.solveConstants 1 t
    let t2 = (fmap . fmap) stepnToOut . Checker.checkTree . Checker.solveConstExpr $ (fmap . fmap) inToStep1 t1
    -- let t3 = inTreeToOut $ cnvTree t2
    -- print t
    putStrLn "---demo"
    prp t2

-- prp t2

-- prp t3
