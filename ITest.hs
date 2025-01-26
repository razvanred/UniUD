module ITest where

import ConstantSolver.Solver
import Control.Monad (join)
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import System.IO
import Text.Pretty.Simple (pPrint)
import TypeChecker.Algs qualified as CS2

f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildBlock pt ()
    putStrLn $ show t
    putStrLn "---"
    pPrint $ CS2.resolveConstantsDemo 1 t
    putStrLn "~~~"
    pPrint $ resolveConstants t
