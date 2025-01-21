import AST2
import ConstantSolving.Esperimenti
import Debug.Trace
import Parser.ASTBuilder2
import Parser.Par (myLexer, pBlock1)

parse x = case pBlock1 (myLexer x) of
    Left _ -> show "error"
    Right t -> show (resolveConstants (buildBlock t ()))

f x = do
    pt <- pBlock1 $ myLexer x
    let t = buildBlock pt ()
    traceM $ show t
    traceM $ show $ resolveConstants t
