import AST
import ConstantSolving.Solver
import Debug.Trace
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TypeChecker.ConstantSolver qualified as ConstantSolver

-- parse x = case pBlock1 (myLexer x) of
--     Left _ -> show "error"
--     Right t -> show (resolveConstants 100 (buildBlock t ()))

f x = do
    pt <- pBlock1 $ myLexer x
    let t = buildBlock pt (Parse ())
    traceM $ show t
    traceM "---"
    traceM $ show $ ConstantSolver.resolveConstants 5 t
    traceM "~~~"
    traceM $ show $ resolveConstants t
