{-# OPTIONS_GHC -w #-}

module ITest where

import ConstantSolver.Solver
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TypeChecker.Algs qualified as CS2
import Utils (prp)

f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildBlock pt ()
    putStrLn $ show t
    putStrLn "---demo"
    prp $ CS2.resolveConstantsDemo 1 t
    putStrLn "~~~actual"
    prp $ resolveConstants t

g x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildBlock pt ()
    prp t
