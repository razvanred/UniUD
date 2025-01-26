module ITest where

import ConstantSolver.Solver
import Control.Monad (join)
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import System.IO
import Text.Pretty.Simple (pPrint)
import TypeChecker.Algs qualified as CS2

-- parse x = case pBlock1 (myLexer x) of
--     Left _ -> show "error"
--     Right t -> show (resolveConstants 100 (buildBlock t ()))

f :: String -> IO ()
f x = do
    let Right pt = pBlock1 $ myLexer x
    let t = buildBlock pt ()
    pPrint $ t
    putStrLn "---"
    pPrint $ CS2.resolveConstantsDemo 1 t
    putStrLn "~~~"
    pPrint $ resolveConstants t
