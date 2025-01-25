module TACtest where
    
import Parser.Abs ()
import Parser.Lex (Token, mkPosToken)
import Parser.Par (myLexer, pBlock1)
import AST
import AST (Type(IntFType))
import TAC.TACgenerator

-- f x = do
--      pt <- pBlock1 $ myLexer x
--      let t = buildBlock pt ()
--      show t
--      show "---"
    
pos = Position 0 0
x = Ident "x"
y = Ident "y"

e1 = (Id pos x (TypeCheckerOutput (PointerFType (IntFType)) (Just RightValue) (Just ModalityVal)))
e2 = (Id pos y (TypeCheckerOutput IntFType (Just LeftValue) (Just ModalityVal)))
e3 = (Deref pos e1 (TypeCheckerOutput IntFType (Just RightValue) (Just ModalityVal)))

a1 = (Assignment pos e1 BasicAssignment e2 (TypeCheckerOutput VoidFType Nothing Nothing))
a2 = (Assignment pos e2 BasicAssignment e3 (TypeCheckerOutput VoidFType Nothing Nothing))
t = tacBlock [a1]
