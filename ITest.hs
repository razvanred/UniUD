-- {-# OPTIONS_GHC -w #-}

module ITest where

import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TypeChecker.Algs qualified as CS2
import TypeChecker.Checker qualified as Checker
import Utils (prp)

f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildTree pt
    let t1 = CS2.solveConstants 1 t
    let t2 = Checker.f t1
    -- print t
    putStrLn "---demo"
    prp t2
