{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Parser.ASTBuilder where

import AST
import Control.Monad (join, liftM2, liftM3)
import Parser.Abs qualified

pass :: b -> a -> b
-- pass = return
pass cons x = cons
pass1 :: (b1 -> a -> b2) -> (a -> b1) -> a -> b2
-- pass1 = (=<<)
pass1 cons1 cons2 x = cons1 (cons2 x) x
pass2 :: (b1 -> b2 -> a -> b3) -> (a -> b1) -> (a -> b2) -> a -> b3
-- pass2 cons1 cons2 cons3 = join $ liftM2 cons1 cons2 cons3
pass2 cons1 cons2 cons3 a = cons1 (cons2 a) (cons3 a) a
pass3 :: (b1 -> b2 -> b3 -> a -> b4) -> (a -> b1) -> (a -> b2) -> (a -> b3) -> a -> b4
-- pass3 cons1 cons2 cons3 cons4 = join $ liftM3 cons1 cons2 cons3 cons4
pass3 cons1 cons2 cons3 cons4 x = cons1 (cons2 x) (cons3 x) (cons4 x) x

buildBlock :: Parser.Abs.Block -> a -> Block a
buildBlock (Parser.Abs.Blck position instructions) x = AST.Block position (flip buildInstruction x <$> instructions) x

buildInstruction :: Parser.Abs.Instruction -> a -> Instruction a
buildInstruction (Parser.Abs.Stmt position statement) = pass1 Statement (buildStatement statement)
buildInstruction (Parser.Abs.Decl position declaration) = pass1 Declaration (buildDeclaration declaration)

buildStatement :: Parser.Abs.Statement -> a -> Statement a
buildStatement (Parser.Abs.Jump _ jumpstatement) = buildJumpStatement jumpstatement
buildStatement (Parser.Abs.Iter _ iterstatement) = buildIterStatement iterstatement
buildStatement (Parser.Abs.Branch _ branchstatement) = buildBranchStatement branchstatement
buildStatement (Parser.Abs.Compound _ block) = pass1 NestedBlock $ buildBlock block
buildStatement (Parser.Abs.StmntExpr position expr) = pass1 (Expression position) (buildExpr expr)
buildStatement (Parser.Abs.Assign position expr1 assignmentop expr2) = \x -> Assignment position (buildExpr expr1 x) (buildAssignment_op assignmentop) (buildExpr expr2 x) x

buildDeclaration :: Parser.Abs.Declaration -> a -> Declaration a
buildDeclaration (Parser.Abs.ConstDecl position ident expr) = pass1 (ConstantDecl position (buildIdent ident)) (buildExpr expr)
buildDeclaration (Parser.Abs.VarDecl position ident declType expr) = pass2 (VariableDecl position (buildIdent ident)) (buildType declType) (buildExpr expr)
buildDeclaration (Parser.Abs.FunDecl position ident parameters declType block) = \x -> FunctionDecl position (buildIdent ident) (flip buildParameter x <$> parameters) (buildType declType x) (buildBlock block x) x

buildJumpStatement :: Parser.Abs.JumpStatement -> a -> Statement a
buildJumpStatement (Parser.Abs.Break position) = Break position
buildJumpStatement (Parser.Abs.Continue position) = Continue position
buildJumpStatement (Parser.Abs.RetExpVoid position) = ReturnVoid position
buildJumpStatement (Parser.Abs.RetExp position expr) = pass1 (ReturnExp position) (buildExpr expr)

buildIdent :: Parser.Abs.Ident -> Ident
buildIdent (Parser.Abs.Ident str) = Ident str

buildParameter :: Parser.Abs.Parameter -> a -> Parameter a
buildParameter (Parser.Abs.Param _ modality ident declType) = pass1 (Param (buildModality modality) (buildIdent ident)) (buildType declType)

buildModality :: Parser.Abs.Modality -> Modality
buildModality (Parser.Abs.Modality1 _) = DefaultByValue
buildModality (Parser.Abs.Modality_val _) = ModalityVal
buildModality (Parser.Abs.Modality_ref _) = ModalityRef

buildType :: Parser.Abs.Type -> a -> DeclType a
buildType (Parser.Abs.BsType _ basicType) = pass $ buildBasicType basicType
buildType (Parser.Abs.ArrayType _ expr declType) = liftM2 ArrayType (Just . buildExpr expr) (buildType declType)
buildType (Parser.Abs.UnsizedArrayType _ declType) = ArrayType Nothing . buildType declType
buildType (Parser.Abs.Pointer _ declType) = PointerType . buildType declType

buildBasicType :: Parser.Abs.BasicType -> DeclType a
buildBasicType (Parser.Abs.BasicType_bool _) = BoolType
buildBasicType (Parser.Abs.BasicType_char _) = CharType
buildBasicType (Parser.Abs.BasicType_int _) = IntType
buildBasicType (Parser.Abs.BasicType_string _) = StringType
buildBasicType (Parser.Abs.BasicType_float _) = FloatType
buildBasicType (Parser.Abs.BasicType_void _) = VoidType

buildAssignment_op :: Parser.Abs.Assignment_op -> AssignmentOp
buildAssignment_op (Parser.Abs.AssignOp _) = BasicAssignment
buildAssignment_op (Parser.Abs.AssignMul _) = AssignMul
buildAssignment_op (Parser.Abs.AssignAdd _) = AssignAdd
buildAssignment_op (Parser.Abs.AssignDiv _) = AssignDiv
buildAssignment_op (Parser.Abs.AssignSub _) = AssignSub
buildAssignment_op (Parser.Abs.AssignPow _) = AssignPow
buildAssignment_op (Parser.Abs.AssignAnd _) = AssignAnd
buildAssignment_op (Parser.Abs.AssignOr _) = AssignOr

buildBranchStatement :: Parser.Abs.BranchStatement -> a -> Statement a
buildBranchStatement (Parser.Abs.If position expr block) = pass2 (IfThen position) (buildExpr expr) (buildBlock block)
buildBranchStatement (Parser.Abs.IfElse position expr block1 block2) = pass3 (IfThenElse position) (buildExpr expr) (buildBlock block1) (buildBlock block2)

buildIterStatement :: Parser.Abs.IterStatement -> a -> Statement a
buildIterStatement (Parser.Abs.While position expr block) = pass2 (While position) (buildExpr expr) (buildBlock block)

buildExpr :: Parser.Abs.Expr -> a -> Expr a
buildExpr (Parser.Abs.Or position expr1 expr2) = pass2 (BinaryOp position $ BooleanOp Or) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.And position expr1 expr2) = pass2 (BinaryOp position $ BooleanOp And) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Not position expr) = pass1 (UnaryOp position Not) (buildExpr expr)
buildExpr (Parser.Abs.Eq position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp Eq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Neq position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp NotEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Lt position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp LessThan) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.LtE position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp LessThanEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Gt position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp GreaterThan) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.GtE position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp GreaterThanEq) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Add position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Add) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Sub position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Sub) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Mul position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Mul) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Div position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Div) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Mod position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Mod) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Pow position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Pow) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Neg position expr) = pass1 (UnaryOp position Neg) (buildExpr expr)
buildExpr (Parser.Abs.PreInc position expr) = pass1 (UnaryOp position PreIncr) (buildExpr expr)
buildExpr (Parser.Abs.PreDecr position expr) = pass1 (UnaryOp position PreDecr) (buildExpr expr)
buildExpr (Parser.Abs.PostInc position expr) = pass1 (UnaryOp position PostIncr) (buildExpr expr)
buildExpr (Parser.Abs.PostDecr position expr) = pass1 (UnaryOp position PostDecr) (buildExpr expr)
buildExpr (Parser.Abs.Ref position expr) = pass1 (Ref position) (buildExpr expr)
buildExpr (Parser.Abs.Deref position expr) = pass1 (Deref position) (buildExpr expr)
buildExpr (Parser.Abs.ArrayAcc position expr1 expr2) = pass2 (ArrayAcc position) (buildExpr expr1) (buildExpr expr2)
buildExpr (Parser.Abs.Id position ident) = Id position (buildIdent ident)
buildExpr (Parser.Abs.FunCall position ident exprs) = \x -> FunctionCall position (buildIdent ident) (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.Int position integer) = pass1 (BasicLiteral position) (IntLiteral integer)
buildExpr (Parser.Abs.Char position char) = pass1 (BasicLiteral position) (CharLiteral char)
buildExpr (Parser.Abs.String position string) = pass1 (BasicLiteral position) (StringLiteral string)
buildExpr (Parser.Abs.Float position double) = pass1 (BasicLiteral position) (FloatLiteral double)
buildExpr (Parser.Abs.Bool position boolean) = pass1 (BasicLiteral position) (buildBoolean boolean)
buildExpr (Parser.Abs.Array position exprs) = \x -> ArrayLiteral position (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.RangedArray position expr1 expr2) = pass2 (RangedArray position) (buildExpr expr1) (buildExpr expr2)

buildBoolean :: Parser.Abs.Boolean -> a -> BasicLiteral a
buildBoolean (Parser.Abs.Boolean_True position) = BoolLiteral True
buildBoolean (Parser.Abs.Boolean_False position) = BoolLiteral False
