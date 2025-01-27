{-# LANGUAGE PatternSynonyms #-}

module Parser.ASTBuilder (buildBlock) where

import AST
import Control.Monad (liftM2)
import Data.Char (toUpper)
import Parser.Abs qualified
import Utils (pass, pass1, pass2, pass3, unexpectedDuring)

-- buildAST :: Parser.Abs.Block -> a -> [Instruction a]
-- buildAST block@(Parser.Abs.Blck pos instructions) = case pos of
--     Just _ -> buildBlock block
--     Nothing -> buildBlock $ Parser.Abs.Blck (Just (0, 0)) instructions

buildFail e@(c : cs) = e `unexpectedDuring` ("build" ++ toUpper (c) : cs)
buildFail "" = "empty string" `unexpectedDuring` "buildFail"

buildBlock :: Parser.Abs.Block -> a -> [Instruction a]
buildBlock (Parser.Abs.Blck _ instructions) x = flip buildInstruction x <$> instructions

buildInstruction :: Parser.Abs.Instruction -> a -> Instruction a
buildInstruction (Parser.Abs.Decl _ declaration) = case declaration of
    (Parser.Abs.ConstDecl (Just pos) ident expr) -> pass1 (ConstantDecl pos (buildIdent ident)) (buildExpr expr)
    (Parser.Abs.VarDecl (Just pos) ident declType expr) -> pass2 (VariableDecl pos (buildIdent ident)) (buildDeclType declType) (buildExpr expr)
    (Parser.Abs.FunDecl (Just pos) ident parameters declType block) -> \x -> FunctionDecl pos (buildIdent ident) (flip buildParameter x <$> parameters) (buildDeclType declType x) (buildBlock block x) x
    _ -> buildFail "instruction"
buildInstruction (Parser.Abs.Stmt _ statement) = case statement of
    (Parser.Abs.Compound (Just pos) block) -> pass1 (NestedBlock pos) (buildBlock block)
    (Parser.Abs.Jump _ jumpstatement) -> case jumpstatement of
        (Parser.Abs.Break (Just pos)) -> Break pos
        (Parser.Abs.Continue (Just pos)) -> Continue pos
        (Parser.Abs.RetExpVoid (Just pos)) -> ReturnVoid pos
        (Parser.Abs.RetExp (Just pos) expr) -> pass1 (ReturnExp pos) (buildExpr expr)
        _ -> fail
    (Parser.Abs.Iter _ iterstatement) -> case iterstatement of
        (Parser.Abs.While (Just pos) expr block) -> pass2 (While pos) (buildExpr expr) (buildBlock block)
        _ -> fail
    (Parser.Abs.Branch _ branchstatement) -> case branchstatement of
        (Parser.Abs.If (Just pos) expr block) -> pass2 (IfThen pos) (buildExpr expr) (buildBlock block)
        (Parser.Abs.IfElse (Just pos) expr block1 block2) -> pass3 (IfThenElse pos) (buildExpr expr) (buildBlock block1) (buildBlock block2)
        _ -> fail
    (Parser.Abs.Assign (Just pos) expr1 assignmentop expr2) -> \x -> Assignment pos (buildExpr expr1 x) (buildAssignment_op assignmentop) (buildExpr expr2 x) x
    (Parser.Abs.StmntExpr (Just pos) expr) -> pass1 (Expression pos) (buildExpr expr)
    _ -> fail
  where
    fail = buildFail "instruction"

buildIdent :: Parser.Abs.Ident -> Ident
buildIdent (Parser.Abs.Ident str) = Ident str

buildDeclType :: Parser.Abs.Type -> a -> DeclType a
buildDeclType (Parser.Abs.BsType _ basicType) = pass $ buildBasicType basicType
buildDeclType (Parser.Abs.ArrayType _ expr declType) = liftM2 ArrayType (Just . buildExpr expr) (buildDeclType declType)
buildDeclType (Parser.Abs.UnsizedArrayType _ declType) = ArrayType Nothing . buildDeclType declType
buildDeclType (Parser.Abs.Pointer _ declType) = PointerType . buildDeclType declType

buildParameter :: Parser.Abs.Parameter -> a -> Parameter a
buildParameter (Parser.Abs.Param _ modality ident declType) = pass1 (Param (buildModality modality) (buildIdent ident)) (buildDeclType declType)

buildModality :: Parser.Abs.Modality -> Modality
buildModality (Parser.Abs.Modality_ref _) = ModalityRef
buildModality _ = ModalityVal

buildExpr :: Parser.Abs.Expr -> a -> Expr a
buildExpr (Parser.Abs.Or (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ BooleanOp Or) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.And (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ BooleanOp And) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Not (Just pos) expr) = pass1 (UnaryOp pos Not) (buildExpr expr)
buildExpr (Parser.Abs.Eq (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp Eq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Neq (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp NotEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Lt (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp LessThan) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.LtE (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp LessThanEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Gt (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp GreaterThan) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.GtE (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ RelationalOp GreaterThanEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Add (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Add) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Sub (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Sub) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Mul (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Mul) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Div (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Div) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Mod (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Mod) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Pow (Just pos) expr1 expr2) = pass2 (BinaryOp pos $ ArithmeticOp Pow) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Neg (Just pos) expr) = pass1 (UnaryOp pos Neg) (buildExpr expr)
buildExpr (Parser.Abs.PreInc (Just pos) expr) = pass1 (UnaryOp pos PreIncr) (buildExpr expr)
buildExpr (Parser.Abs.PreDecr (Just pos) expr) = pass1 (UnaryOp pos PreDecr) (buildExpr expr)
buildExpr (Parser.Abs.PostInc (Just pos) expr) = pass1 (UnaryOp pos PostIncr) (buildExpr expr)
buildExpr (Parser.Abs.PostDecr (Just pos) expr) = pass1 (UnaryOp pos PostDecr) (buildExpr expr)
buildExpr (Parser.Abs.Ref (Just pos) expr) = pass1 (Ref pos) (buildExpr expr)
buildExpr (Parser.Abs.Deref (Just pos) expr) = pass1 (Deref pos) (buildExpr expr)
buildExpr (Parser.Abs.ArrayAcc (Just pos) expr1 expr2) = pass2 (ArrayAcc pos) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Id (Just pos) ident) = Id pos (buildIdent ident)
buildExpr (Parser.Abs.FunCall (Just pos) ident exprs) = \x -> FunctionCall pos (buildIdent ident) (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.Int (Just pos) integer) = pass1 (BasicLiteral pos) (IntLiteral integer)
buildExpr (Parser.Abs.Char (Just pos) char) = pass1 (BasicLiteral pos) (CharLiteral char)
buildExpr (Parser.Abs.String (Just pos) string) = pass1 (BasicLiteral pos) (StringLiteral string)
buildExpr (Parser.Abs.Float (Just pos) double) = pass1 (BasicLiteral pos) (FloatLiteral double)
buildExpr (Parser.Abs.Bool (Just pos) boolean) = pass1 (BasicLiteral pos) (buildBoolean boolean)
buildExpr (Parser.Abs.Array (Just pos) exprs) = \x -> ArrayLiteral pos (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.RangedArray (Just pos) expr1 expr2) = pass2 (RangedArray pos) (buildExpr expr1) (buildExpr expr2)
buildExpr _ = "expression" `unexpectedDuring` "buildExpr"

buildBasicType :: Parser.Abs.BasicType -> DeclType a
buildBasicType (Parser.Abs.BasicType_bool _) = BoolType
buildBasicType (Parser.Abs.BasicType_char _) = CharType
buildBasicType (Parser.Abs.BasicType_int _) = IntType
buildBasicType (Parser.Abs.BasicType_string _) = StringType
buildBasicType (Parser.Abs.BasicType_float _) = FloatType
buildBasicType (Parser.Abs.BasicType_void _) = VoidType

buildBoolean :: Parser.Abs.Boolean -> a -> BasicLiteral a
buildBoolean (Parser.Abs.Boolean_True _) = BoolLiteral True
buildBoolean (Parser.Abs.Boolean_False _) = BoolLiteral False

buildAssignment_op :: Parser.Abs.Assignment_op -> AssignmentOp
buildAssignment_op (Parser.Abs.AssignOp _) = BasicAssignment
buildAssignment_op (Parser.Abs.AssignMul _) = AssignMul
buildAssignment_op (Parser.Abs.AssignAdd _) = AssignAdd
buildAssignment_op (Parser.Abs.AssignDiv _) = AssignDiv
buildAssignment_op (Parser.Abs.AssignSub _) = AssignSub
buildAssignment_op (Parser.Abs.AssignPow _) = AssignPow
buildAssignment_op (Parser.Abs.AssignAnd _) = AssignAnd
buildAssignment_op (Parser.Abs.AssignOr _) = AssignOr
