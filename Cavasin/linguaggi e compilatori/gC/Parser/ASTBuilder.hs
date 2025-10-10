{-# LANGUAGE PatternSynonyms #-}

module Parser.ASTBuilder (buildTree) where

import AST
import Control.Monad.Extra (void)
import Data.Char (isUpper, toUpper)
import Data.Set qualified as Set
import Parser.Abs qualified
import Utils

buildTree :: Parser.Abs.Block -> [Instruction ParserOutput]
-- unfortunate choices
buildTree block = buildBlock block ParserOutput{pswarnings = Set.empty}

buildFail e@(c : cs) = e `unexpectedDuring` "build" ++ toUpper (c) : cs
buildFail "" = "empty string" `unexpectedDuring` "buildFail"

buildBlock :: Parser.Abs.Block -> ParserOutput -> [Instruction ParserOutput]
buildBlock (Parser.Abs.Blck _ instructions) x = flip buildInstruction x <$> instructions

buildInstruction :: Parser.Abs.Instruction -> ParserOutput -> Instruction ParserOutput
buildInstruction (Parser.Abs.Decl _ declaration) = case declaration of
    (Parser.Abs.ConstDecl (Just pos) (Parser.Abs.Ident ident) expr) ->
        ( if all isUpper ident
            then
                id
            else
                (pass1 (|<) (LowercaseConstant . void))
        )
            . pass1 (ConstantDecl pos ident) (buildExpr expr)
    (Parser.Abs.VarDecl (Just pos) (Parser.Abs.Ident ident@(h : _)) declType expr) ->
        ( if isUpper h
            then
                (UpperCaseSymbol |<)
            else
                id
        )
            . pass2 (VariableDecl pos ident) (buildDeclType declType) (buildExpr expr)
    (Parser.Abs.FunDecl (Just pos) (Parser.Abs.Ident ident) parameters declType block) -> \x -> FunctionDecl pos ident (flip buildParameter x <$> parameters) (buildDeclType declType x) (buildBlock block x) x
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
        (Parser.Abs.DoWhile (Just pos) block expr) -> pass2 (DoWhile pos) (buildExpr expr) (buildBlock block)
        (Parser.Abs.For (Just pos) (Parser.Abs.Ident ident) expr1 expr2 expr3 block) -> pass4 (For pos ident) (buildExpr expr1) (buildExpr expr2) (buildExpr expr3) (buildBlock block)
        _ -> fail
    (Parser.Abs.Branch _ branchstatement) -> case branchstatement of
        (Parser.Abs.If (Just pos) expr block) -> pass2 (IfThen pos) (buildExpr expr) (buildBlock block)
        (Parser.Abs.IfElse (Just pos) expr block1 block2) -> pass3 (IfThenElse pos) (buildExpr expr) (buildBlock block1) (buildBlock block2)
        _ -> fail
    (Parser.Abs.Assign (Just pos) expr1 assignmentop expr2) -> \x -> Assignment pos (buildExpr expr1 x) (buildAssignment_op assignmentop) (buildExpr expr2 x) x
    (Parser.Abs.StmntExpr (Just pos) expr) -> pass1 (Expression pos) (buildExpr expr)
    (Parser.Abs.TryStmt (Just pos) block1 block2) -> \x -> TryCatch pos (buildBlock block1 x) (buildBlock block2 x) x
    (Parser.Abs.Throw (Just pos) string) -> \x -> Throw pos string x
    _ -> fail
    where
        fail = buildFail "instruction"

-- buildIdent :: Parser.Abs.Ident -> Ident
-- buildIdent (Parser.Abs.Ident str) = toUpper <$> str

buildDeclType :: Parser.Abs.Type -> ParserOutput -> DeclType ParserOutput
buildDeclType (Parser.Abs.BsType _ basicType) = (\_ -> buildBasicType basicType)
buildDeclType (Parser.Abs.ArrayType _ expr declType) = liftA2 (DArrayType False) (Just . buildExpr expr) (buildDeclType declType)
buildDeclType (Parser.Abs.CArrType _ expr declType) = liftA2 (DArrayType True) (Just . buildExpr expr) (buildDeclType declType)
buildDeclType (Parser.Abs.UnsizedArrayType _ declType) = DArrayType False Nothing . buildDeclType declType
buildDeclType (Parser.Abs.UnsizedCArrayType _ declType) = DArrayType True Nothing . buildDeclType declType
buildDeclType (Parser.Abs.Pointer _ declType) = DPointerType . buildDeclType declType

buildParameter :: Parser.Abs.Parameter -> ParserOutput -> Parameter ParserOutput
buildParameter (Parser.Abs.Param (Just pos) modality (Parser.Abs.Ident ident) declType) = pass1 (Param pos (buildModality modality) ident) (buildDeclType declType)
buildParameter p = buildFail ("parameter" ++ show p)

buildModality :: Parser.Abs.Modality -> Modality
buildModality (Parser.Abs.Modality_val _) = ModalityVal
buildModality (Parser.Abs.Modality_ref _) = ModalityRef
buildModality (Parser.Abs.Modality_res _) = ModalityRes
buildModality (Parser.Abs.Modality_valres _) = ModalityValRes
buildModality (Parser.Abs.Modality1 _) = ModalityVal -- it was "modality" `unexpectedDuring` "buildModality"

buildExpr :: Parser.Abs.Expr -> ParserOutput -> Expr ParserOutput
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
buildExpr (Parser.Abs.Id (Just pos) (Parser.Abs.Ident ident)) = Id pos ident
buildExpr (Parser.Abs.FunCall (Just pos) (Parser.Abs.Ident ident) exprs) = \x -> FunctionCall pos ident (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.Int (Just pos) integer) = IntLiteral pos integer
buildExpr (Parser.Abs.Char (Just pos) char) = CharLiteral pos char
buildExpr (Parser.Abs.String (Just pos) string) = StringLiteral pos string
buildExpr (Parser.Abs.Float (Just pos) double) = FloatLiteral pos double
buildExpr (Parser.Abs.Bool (Just pos) boolean) = BoolLiteral pos (buildBoolean boolean)
buildExpr (Parser.Abs.Array (Just pos) exprs) = \x -> ArrayLiteral pos (flip buildExpr x <$> exprs) x
buildExpr (Parser.Abs.IfExpr (Just pos) expr1 expr2 expr3) = pass3 (CondExpr pos) (buildExpr expr1) (buildExpr expr2) (buildExpr expr3)
buildExpr _ = buildFail "expr"

buildBasicType :: Parser.Abs.BasicType -> DeclType ParserOutput
buildBasicType (Parser.Abs.BasicType_bool _) = DBoolType
buildBasicType (Parser.Abs.BasicType_char _) = DCharType
buildBasicType (Parser.Abs.BasicType_int _) = DIntType
buildBasicType (Parser.Abs.BasicType_string _) = DStringType
buildBasicType (Parser.Abs.BasicType_float _) = DFloatType
buildBasicType (Parser.Abs.BasicType_void _) = DVoidType

buildBoolean :: Parser.Abs.Boolean -> Bool
buildBoolean (Parser.Abs.Boolean_True _) = True
buildBoolean (Parser.Abs.Boolean_False _) = False

buildAssignment_op :: Parser.Abs.Assignment_op -> AssignmentOp
buildAssignment_op (Parser.Abs.AssignOp _) = BasicAssignment
buildAssignment_op (Parser.Abs.AssignMul _) = AssignMul
buildAssignment_op (Parser.Abs.AssignAdd _) = AssignAdd
buildAssignment_op (Parser.Abs.AssignDiv _) = AssignDiv
buildAssignment_op (Parser.Abs.AssignSub _) = AssignSub
buildAssignment_op (Parser.Abs.AssignPow _) = AssignPow
buildAssignment_op (Parser.Abs.AssignAnd _) = AssignAnd
buildAssignment_op (Parser.Abs.AssignOr _) = AssignOr
