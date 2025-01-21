import AST2
import Debug.Trace
import Parser.ASTBuilder2
import Parser.Par (myLexer, pBlock1)
import TypeChecker.ConstantSolver qualified

-- import TypeChecker.ConstantSolver
import ConstantSolving.Solver
import ConstantSolving.Solver qualified as TypeChecker

-- parse x = case pBlock1 (myLexer x) of
--     Left _ -> show "error"
--     Right t -> show (resolveConstants 100 (buildBlock t ()))

f x = do
    pt <- pBlock1 $ myLexer x
    let t = buildBlock pt (Parse ())
    traceM $ show t
    traceM "---"
    traceM $ show $ TypeChecker.ConstantSolver.resolveConstants 5 t
    traceM "~~~"
    traceM $ show $ resolveConstants t
