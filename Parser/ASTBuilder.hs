{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Parser.ASTBuilder where

import AST
import Control.Monad (join, liftM2, liftM3)
import Parser.Abs qualified

pass :: b -> a -> b
pass = return
pass1 :: (b1 -> a -> b2) -> (a -> b1) -> a -> b2
pass1 = (=<<)
pass2 :: (b1 -> b2 -> a -> b3) -> (a -> b1) -> (a -> b2) -> a -> b3
pass2 cons1 cons2 cons3 = join $ liftM2 cons1 cons2 cons3

pass3 :: (b1 -> b2 -> b3 -> a -> b4) -> (a -> b1) -> (a -> b2) -> (a -> b3) -> a -> b4
pass3 cons1 cons2 cons3 cons4 = join $ liftM3 cons1 cons2 cons3 cons4

transBlock :: Parser.Abs.Block -> a -> Block a
transBlock (Parser.Abs.Blck position instructions) x = AST.Block position (flip transInstruction x <$> instructions) x

transInstruction :: Parser.Abs.Instruction -> a -> Instruction a
transInstruction (Parser.Abs.Stmt position statement) = pass1 (Statement position) (transStatement statement)
transInstruction (Parser.Abs.Decl position declaration) = pass1 (Declaration position) (transDeclaration declaration)

transStatement :: Parser.Abs.Statement -> a -> Statement a
transStatement (Parser.Abs.Jump _ jumpstatement) = transJumpStatement jumpstatement
transStatement (Parser.Abs.Iter _ iterstatement) = transIterStatement iterstatement
transStatement (Parser.Abs.Branch _ branchstatement) = transBranchStatement branchstatement
transStatement (Parser.Abs.Compound _ block) = pass1 NestedBlock $ transBlock block
transStatement (Parser.Abs.StmntExpr position expr) = pass1 (Expression position) (transExpr expr)
transStatement (Parser.Abs.Assign position expr1 assignmentop expr2) = \x -> Assignment position (transExpr expr1 x) (transAssignment_op assignmentop) (transExpr expr2 x) x

transDeclaration :: Parser.Abs.Declaration -> a -> Declaration a
transDeclaration (Parser.Abs.ConstDecl position ident expr) = pass1 (ConstantDecl position (transIdent ident)) (transExpr expr)
transDeclaration (Parser.Abs.VarDecl position ident declType expr) = pass2 (VariableDecl position (transIdent ident)) (transType declType) (transExpr expr)
transDeclaration (Parser.Abs.FunDecl position ident parameters declType block) = \x -> FunctionDecl position (transIdent ident) (flip transParameter x <$> parameters) (transType declType x) (transBlock block x) x

transJumpStatement :: Parser.Abs.JumpStatement -> a -> Statement a
transJumpStatement (Parser.Abs.Break position) = Break position
transJumpStatement (Parser.Abs.Continue position) = Continue position
transJumpStatement (Parser.Abs.RetExpVoid position) = ReturnVoid position
transJumpStatement (Parser.Abs.RetExp position expr) = pass1 (ReturnExp position) (transExpr expr)

transIdent :: Parser.Abs.Ident -> Ident
transIdent (Parser.Abs.Ident str) = Ident str

transParameter :: Parser.Abs.Parameter -> a -> Parameter a
transParameter (Parser.Abs.Param _ modality ident declType) = pass1 (Param (transModality modality) (transIdent ident)) (transType declType)

transModality :: Parser.Abs.Modality -> Modality
transModality (Parser.Abs.Modality1 _) = DefaultByValue
transModality (Parser.Abs.Modality_val _) = ModalityVal
transModality (Parser.Abs.Modality_ref _) = ModalityRef

transType :: Parser.Abs.Type -> a -> Type a
transType (Parser.Abs.BsType _ basicType) = pass $ transBasicType basicType
transType (Parser.Abs.ArrayType _ expr declType) = liftM2 ArrayType (Just . transExpr expr) (transType declType)
transType (Parser.Abs.UnsizedArrayType _ declType) = ArrayType Nothing . transType declType
transType (Parser.Abs.Pointer _ declType) = Pointer . transType declType

transBasicType :: Parser.Abs.BasicType -> Type a
transBasicType (Parser.Abs.BasicType_bool _) = BoolType
transBasicType (Parser.Abs.BasicType_char _) = CharType
transBasicType (Parser.Abs.BasicType_int _) = IntType
transBasicType (Parser.Abs.BasicType_string _) = StringType
transBasicType (Parser.Abs.BasicType_float _) = FloatType
transBasicType (Parser.Abs.BasicType_void _) = VoidType

transAssignment_op :: Parser.Abs.Assignment_op -> AssignmentOp
transAssignment_op (Parser.Abs.AssignOp _) = BasicAssignment
transAssignment_op (Parser.Abs.AssignMul _) = AssignMul
transAssignment_op (Parser.Abs.AssignAdd _) = AssignAdd
transAssignment_op (Parser.Abs.AssignDiv _) = AssignDiv
transAssignment_op (Parser.Abs.AssignSub _) = AssignSub
transAssignment_op (Parser.Abs.AssignPow _) = AssignPow
transAssignment_op (Parser.Abs.AssignAnd _) = AssignAnd
transAssignment_op (Parser.Abs.AssignOr _) = AssignOr

transBranchStatement :: Parser.Abs.BranchStatement -> a -> Statement a
transBranchStatement (Parser.Abs.If position expr block) = pass2 (IfThen position) (transExpr expr) (transBlock block)
transBranchStatement (Parser.Abs.IfElse position expr block1 block2) = pass3 (IfThenElse position) (transExpr expr) (transBlock block1) (transBlock block2)

transIterStatement :: Parser.Abs.IterStatement -> a -> Statement a
transIterStatement (Parser.Abs.While position expr block) = pass2 (While position) (transExpr expr) (transBlock block)

transExpr :: Parser.Abs.Expr -> a -> Expr a
transExpr (Parser.Abs.Or position expr1 expr2) = pass2 (BinaryOp position $ BooleanOp Or) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.And position expr1 expr2) = pass2 (BinaryOp position $ BooleanOp And) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Not position expr) = pass1 (UnaryOp position Not) (transExpr expr)
transExpr (Parser.Abs.Eq position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp Eq) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Neq position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp NotEq) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Lt position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp LessThan) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.LtE position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp LessThanEq) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Gt position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp GreaterThan) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.GtE position expr1 expr2) = pass2 (BinaryOp position $ RelationalOp GreaterThanEq) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Add position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Add) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Sub position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Sub) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Mul position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Mul) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Div position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Div) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Mod position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Mod) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Pow position expr1 expr2) = pass2 (BinaryOp position $ ArithmeticOp Pow) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Neg position expr) = pass1 (UnaryOp position Neg) (transExpr expr)
transExpr (Parser.Abs.PreInc position expr) = pass1 (UnaryOp position PreIncr) (transExpr expr)
transExpr (Parser.Abs.PreDecr position expr) = pass1 (UnaryOp position PreDecr) (transExpr expr)
transExpr (Parser.Abs.PostInc position expr) = pass1 (UnaryOp position PostIncr) (transExpr expr)
transExpr (Parser.Abs.PostDecr position expr) = pass1 (UnaryOp position PostDecr) (transExpr expr)
transExpr (Parser.Abs.Ref position expr) = pass1 (Ref position) (transExpr expr)
transExpr (Parser.Abs.Deref position expr) = pass1 (Deref position) (transExpr expr)
transExpr (Parser.Abs.ArrayAcc position expr1 expr2) = pass2 (ArrayAcc position) (transExpr expr1) (transExpr expr2)
transExpr (Parser.Abs.Id position ident) = Id position (transIdent ident)
transExpr (Parser.Abs.FunCall position ident exprs) = \x -> FunctionCall position (transIdent ident) (flip transExpr x <$> exprs) x
transExpr (Parser.Abs.Int position integer) = pass1 (BasicLiteral position) (IntLiteral integer)
transExpr (Parser.Abs.Char position char) = pass1 (BasicLiteral position) (CharLiteral char)
transExpr (Parser.Abs.String position string) = pass1 (BasicLiteral position) (StringLiteral string)
transExpr (Parser.Abs.Float position double) = pass1 (BasicLiteral position) (FloatLiteral double)
transExpr (Parser.Abs.Bool position boolean) = pass1 (BasicLiteral position) (transBoolean boolean)
transExpr (Parser.Abs.Array position exprs) = \x -> ArrayLiteral position (flip transExpr x <$> exprs) x
transExpr (Parser.Abs.RangedArray position expr1 expr2) = pass2 (RangedArray position) (transExpr expr1) (transExpr expr2)

transBoolean :: Parser.Abs.Boolean -> a -> BasicLiteral a
transBoolean (Parser.Abs.Boolean_True position) = BoolLiteral True
transBoolean (Parser.Abs.Boolean_False position) = BoolLiteral False
