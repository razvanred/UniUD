{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid partial function" #-}
module Test where

import ConstantSolver.Solver
import ErrorCollector.Main
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import System.Environment (getArgs)
import TAC.TACInstruction
import TAC.TACutils (printOutput)
import TypeChecker.Main
import Utils (prp)

main :: IO ()
main = do
    filename <- getArgs
    contents <- readFile (head filename)
    putStrLn "########## PROGRAMMA SORGENTE ##########"
    putStrLn contents
    putStrLn "##########        TAC         ##########"
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
