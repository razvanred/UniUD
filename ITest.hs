{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module ITest where

import ConstantSolver.Solver
import ErrorCollector.Main
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TAC.TACInstruction
import TAC.TACutils (genCode, printOutput, printProgram)
import TypeChecker.Main
import Utils (prp)

f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildTree pt
    let t1 = solveConstants 1 t
    let t2 = staticAnalizer t1
    let Right t3 = collectErrors True t2
    let t4 = tacBlock t3
    -- print t
    putStrLn "---demo"
    -- prp t2
    prp t3

-- printOutput t4

testFile fileName = do
    contents <- readFile fileName
    f contents
