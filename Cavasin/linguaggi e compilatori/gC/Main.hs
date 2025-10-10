module Main where

import ConstantSolver.Solver
import ErrorCollector.Main
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TAC.TACInstruction
import TAC.TACutils (printOutput)
import TypeChecker.Main
import Utils

main :: IO ()
main = do
    contents <- readFile "tests/demo.txt"
    putStrLn "########## PROGRAMMA SORGENTE ##########"
    putStrLn contents
    putStrLn "##########        TAC         ##########"
    f contents

testFile :: FilePath -> IO ()
testFile fileName = do
    contents <- readFile fileName
    f contents

f :: String -> IO ()
f x = do
    case pBlock1 $ myLexer x of
        Left x -> putStrLn x
        Right pt -> do
            let t = buildTree pt
            let t1 = solveConstants 3 t
            let t2 = staticAnalizer t1
            case collectErrors True t2 of
                Left msgs -> do
                    -- prp t2
                    prp msgs
                Right t3 -> do
                    let t4 = tacProgram t3
                    putStrLn "---demo"
                    -- prp t3
                    printOutput t4
