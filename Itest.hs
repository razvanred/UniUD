module ITest where

import AST
import ConstantSolving.Solver
import Debug.Trace
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TypeChecker.Algs qualified as CS2

-- parse x = case pBlock1 (myLexer x) of
--     Left _ -> show "error"
--     Right t -> show (resolveConstants 100 (buildBlock t ()))

f x = do
    pt <- pBlock1 $ myLexer x
    let t = buildBlock pt (Parse ())
    traceM $ show t
    traceM "---"
    traceM $ show $ CS2.resolveConstants 5 t
    traceM "~~~"
    traceM $ show $ resolveConstants t
