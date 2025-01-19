import AST
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)

parse x = case pBlock1 (myLexer x) of
    Left _ -> show "error"
    Right bl -> show (transBlock bl ())

-- a = Block (Just (1,1)) () [Statement (Just (1,1)) () (Expression (Just (1,1)) () (BinaryOp (Just (1,1)) () (ArithmeticOp Add) (BasicLiteral (Just (1,1)) () (IntLiteral () 1)) (BasicLiteral (Just (1,3)) () (IntLiteral () 1)))),Statement (Just (1,5)) () (Expression (Just (1,5)) () (BinaryOp (Just (1,5)) () (ArithmeticOp Add) (BasicLiteral (Just (1,5)) () (IntLiteral () 3)) (BasicLiteral (Just (1,7)) () (IntLiteral () 4)))),Declaration (Just (1,9)) () (VariableDecl (Just (1,9)) () (Ident "x") IntType (ArrayLiteral (Just (1,23)) () [BasicLiteral (Just (1,24)) () (IntLiteral () 1),BasicLiteral (Just (1,26)) () (IntLiteral () 2),BasicLiteral (Just (1,28)) () (IntLiteral () 3)]))]
