{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Parser.ASTBuilder where

import AST
import Parser.Abs qualified

-- import Prelude (Bool, Char, Double, Either (..), Integer, Maybe (..), Show, String, show, ($), (++))

-- type Err = Either String
-- type Result = Err String

-- failure :: (Show a) => a -> Result
-- failure x = Left $ "Undefined case: " ++ show x

transIdent :: Parser.Abs.Ident -> Ident
transIdent (Parser.Abs.Ident str) = Ident str

transBlock :: (Show a) => Parser.Abs.Block -> a -> Block a
transBlock (Parser.Abs.Blck position instructions) x = AST.Block position x (map (\y -> transInstruction y x) instructions)

transInstruction :: (Show a) => Parser.Abs.Instruction -> a -> Instruction a
transInstruction y x = case y of
    Parser.Abs.Stmt position statement -> Statement position x (transStatement statement x)
    Parser.Abs.Decl position declaration -> Declaration position x (transDeclaration declaration x)

transDeclaration :: (Show a) => Parser.Abs.Declaration -> a -> Declaration a
transDeclaration y x = case y of
    Parser.Abs.ConstDecl position ident expr -> ConstantDecl position x (transIdent ident) (transExpr expr x)
    Parser.Abs.VarDecl position ident type_ expr -> VariableDecl position x (transIdent ident) (transType type_ x) (transExpr expr x)
    Parser.Abs.FunDecl position ident parameters type_ block -> FunctionDecl position x (transIdent ident) (map (\y -> transParameter y x) parameters) (transType type_ x) (transBlock block x)

transParameter :: (Show a) => Parser.Abs.Parameter -> a -> Parameter a
transParameter y x = case y of
    Parser.Abs.Param _ modality ident type_ -> Param x (transModality modality) (transIdent ident) (transType type_ x)

transModality :: Parser.Abs.Modality -> Modality
transModality y = case y of
    Parser.Abs.Modality1 _ -> DefaultByValue
    Parser.Abs.Modality_val _ -> ModalityVal
    Parser.Abs.Modality_ref _ -> ModalityRef

transType :: (Show a) => Parser.Abs.Type -> a -> Type a
transType y x = case y of
    Parser.Abs.BsType _ basicType -> transBasicType basicType
    Parser.Abs.ArrayType _ expr type_ -> ArrayType (Just (transExpr expr x)) (transType type_ x)
    Parser.Abs.UnsizedArrayType _ type_ -> ArrayType Nothing (transType type_ x)
    Parser.Abs.Pointer _ type_ -> Pointer (transType type_ x)

transBasicType :: Parser.Abs.BasicType -> Type a
transBasicType y = case y of
    Parser.Abs.BasicType_bool _ -> BoolType
    Parser.Abs.BasicType_char _ -> CharType
    Parser.Abs.BasicType_int _ -> IntType
    Parser.Abs.BasicType_string _ -> StringType
    Parser.Abs.BasicType_float _ -> FloatType
    Parser.Abs.BasicType_void _ -> VoidType

transStatement :: (Show a) => Parser.Abs.Statement -> a -> Statement a
transStatement y x = case y of
    Parser.Abs.Compound position block -> NestedBlock position x (transBlock block x)
    Parser.Abs.Jump position jumpstatement -> transJumpStatement jumpstatement x
    Parser.Abs.Iter position iterstatement -> transIterStatement iterstatement x
    Parser.Abs.Branch position branchstatement -> transBranchStatement branchstatement x
    Parser.Abs.Assign position expr1 assignmentop expr2 -> Assignment position x (transExpr expr1 x) (transAssignment_op assignmentop) (transExpr expr2 x)
    Parser.Abs.StmntExpr position expr -> Expression position x (transExpr expr x)

transAssignment_op :: Parser.Abs.Assignment_op -> AssignmentOp
transAssignment_op y = case y of
    Parser.Abs.AssignOp _ -> BasicAssignment
    Parser.Abs.AssignMul _ -> AssignMul
    Parser.Abs.AssignAdd _ -> AssignAdd
    Parser.Abs.AssignDiv _ -> AssignDiv
    Parser.Abs.AssignSub _ -> AssignSub
    Parser.Abs.AssignPow _ -> AssignPow
    Parser.Abs.AssignAnd _ -> AssignAnd
    Parser.Abs.AssignOr _ -> AssignOr

transJumpStatement :: (Show a) => Parser.Abs.JumpStatement -> a -> Statement a
transJumpStatement y x = case y of
    Parser.Abs.Break position -> Break position x
    Parser.Abs.Continue position -> Continue position x
    Parser.Abs.RetExpVoid position -> ReturnVoid position x
    Parser.Abs.RetExp position expr -> ReturnExp position x (transExpr expr x)

transBranchStatement :: (Show a) => Parser.Abs.BranchStatement -> a -> Statement a
transBranchStatement y x = case y of
    Parser.Abs.If position expr block -> IfThen position x (transExpr expr x) (transBlock block x)
    Parser.Abs.IfElse position expr block1 block2 -> IfThenElse position x (transExpr expr x) (transBlock block1 x) (transBlock block2 x)

transIterStatement :: (Show a) => Parser.Abs.IterStatement -> a -> Statement a
transIterStatement y x = case y of
    Parser.Abs.While position expr block -> While position x (transExpr expr x) (transBlock block x)

transExpr :: (Show a) => Parser.Abs.Expr -> a -> Expr a
transExpr y x = case y of
    Parser.Abs.Or position expr1 expr2 -> BinaryOp position x (BooleanOp Or) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.And position expr1 expr2 -> BinaryOp position x (BooleanOp And) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Not position expr -> UnaryOp position x Not (transExpr expr x)
    Parser.Abs.Eq position expr1 expr2 -> BinaryOp position x (RelationalOp Eq) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Neq position expr1 expr2 -> BinaryOp position x (RelationalOp NotEq) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Lt position expr1 expr2 -> BinaryOp position x (RelationalOp LessThan) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.LtE position expr1 expr2 -> BinaryOp position x (RelationalOp LessThanEq) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Gt position expr1 expr2 -> BinaryOp position x (RelationalOp GreaterThan) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.GtE position expr1 expr2 -> BinaryOp position x (RelationalOp GreaterThanEq) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Add position expr1 expr2 -> BinaryOp position x (ArithmeticOp Add) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Sub position expr1 expr2 -> BinaryOp position x (ArithmeticOp Sub) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Mul position expr1 expr2 -> BinaryOp position x (ArithmeticOp Mul) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Div position expr1 expr2 -> BinaryOp position x (ArithmeticOp Div) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Mod position expr1 expr2 -> BinaryOp position x (ArithmeticOp Mod) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Pow position expr1 expr2 -> BinaryOp position x (ArithmeticOp Pow) (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Neg position expr -> UnaryOp position x Neg (transExpr expr x)
    Parser.Abs.PreInc position expr -> UnaryOp position x PreIncr (transExpr expr x)
    Parser.Abs.PreDecr position expr -> UnaryOp position x PreDecr (transExpr expr x)
    Parser.Abs.PostInc position expr -> UnaryOp position x PostIncr (transExpr expr x)
    Parser.Abs.PostDecr position expr -> UnaryOp position x PostDecr (transExpr expr x)
    Parser.Abs.Ref position expr -> Ref position x (transExpr expr x)
    Parser.Abs.Deref position expr -> Deref position x (transExpr expr x)
    Parser.Abs.ArrayAcc position expr1 expr2 -> ArrayAcc position x (transExpr expr1 x) (transExpr expr2 x)
    Parser.Abs.Id position ident -> Id position x (transIdent ident)
    Parser.Abs.FunCall position ident exprs -> FunctionCall position x (transIdent ident) (map (\y -> transExpr y x) exprs)
    Parser.Abs.Int position integer -> BasicLiteral position x (IntLiteral x integer)
    Parser.Abs.Char position char -> BasicLiteral position x (CharLiteral x char)
    Parser.Abs.String position string -> BasicLiteral position x (StringLiteral x string)
    Parser.Abs.Float position double -> BasicLiteral position x (FloatLiteral x double)
    Parser.Abs.Bool position boolean -> BasicLiteral position x (transBoolean boolean x)
    Parser.Abs.Array position exprs -> ArrayLiteral position x (map (\y -> transExpr y x) exprs)
    Parser.Abs.RangedArray position expr1 expr2 -> RangedArray position x (transExpr expr1 x) (transExpr expr2 x)

transBoolean :: (Show a) => Parser.Abs.Boolean -> a -> BasicLiteral a
transBoolean (Parser.Abs.Boolean_True position) x = BoolLiteral x True
transBoolean (Parser.Abs.Boolean_False position) x = BoolLiteral x False
