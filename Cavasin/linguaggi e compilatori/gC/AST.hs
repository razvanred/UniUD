{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | The abstract syntax of language Parser.
module AST where

import Algebra.Lattice (BoundedMeetSemiLattice (top), Lattice ((\/)))
import Algebra.Lattice qualified
import Data.Foldable (fold)
import Data.Function (on)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (mapAccumL, mapAccumR)
import Utils

data Error
    = MaxRecursion -- migrates (not really)
    | TypeMismatch Type (Either Type String) -- halts
    | UnsolvableType
    | UnknownSymbol
    | ArgCount
    | EmptyArray
    | VariableAlreadyDefined (Instruction ())
    | FunctionAlreadyDefined (Instruction ())
    | JumpOutsideLoop
    | NonConstExpr
    | NonTotalFunction
    | DisallowedLiteral
    | DisallowedType
    | ThrowOutsideTry
    deriving (Show)

errorRank = \case
    MaxRecursion{} -> "1"
    TypeMismatch tpe1 tpe2 -> "2" ++ show tpe1 ++ show tpe2
    UnsolvableType -> "3"
    UnknownSymbol -> "4"
    ArgCount -> "5"
    EmptyArray -> "6"
    VariableAlreadyDefined{} -> "7"
    FunctionAlreadyDefined{} -> "8"
    JumpOutsideLoop -> "9"
    NonTotalFunction -> "10"
    NonConstExpr -> "11"
    DisallowedLiteral -> "12"
    DisallowedType -> "13"
    ThrowOutsideTry -> "14"

instance Eq Error where
    (==) = (==) `on` errorRank

instance Ord Error where
    compare = compare `on` errorRank

data Warning
    = LowercaseConstant (Instruction ()) -- only constdecls (csReplacedFromConstant)
    | UpperCaseSymbol -- only vardecls
    | ConstantAlreadyDefined (Instruction ()) -- only constdecls
    | LiteralOutOfRange (Expr ()) -- migrates
    | DivisionBy0
    | UnreachableCode
    deriving (Show)

warningRank = \case
    LowercaseConstant decl -> "1" ++ show decl
    UpperCaseSymbol -> "2"
    ConstantAlreadyDefined decl -> "3" ++ show decl
    LiteralOutOfRange expr -> "4" ++ show expr
    DivisionBy0 -> "5"
    UnreachableCode -> "6"

instance Eq Warning where
    (==) = (==) `on` warningRank

instance Ord Warning where
    compare = compare `on` warningRank

data ParserOutput = ParserOutput
    { pswarnings :: Set Warning
    }
    deriving (Show)

instance StatusCollector Warning ParserOutput where
    w |< parserOutput = parserOutput{pswarnings = Set.insert w (pswarnings parserOutput)}

data ConstantSolverOutput = ConstantSolverOutput
    { cserrors :: Set Error,
      cswarnings :: Set Warning,
      csReplacedFromConstant :: Maybe (Instruction ConstantSolverOutput)
    }
    deriving (Show)

instance StatusCollector Error ConstantSolverOutput where
    e |< constantSolverOutput = constantSolverOutput{cserrors = Set.insert e (cserrors constantSolverOutput)}

instance StatusCollector Warning ConstantSolverOutput where
    w |< constantSolverOutput = constantSolverOutput{cswarnings = Set.insert w (cswarnings constantSolverOutput)}

data TypeCheckerOutput = TypeCheckerOutput
    { tcerrors :: Set Error,
      tcwarnings :: Set Warning,
      tcReplacedFromConstant :: Maybe (Instruction ConstantSolverOutput),
      tcType :: Type,
      tcSide :: LeftRightValue,
      tcBinding :: Maybe (Int, Modality, Instruction ())
    }
    deriving (Show)

instance StatusCollector Error TypeCheckerOutput where
    e |< constantSolverOutput = constantSolverOutput{tcerrors = Set.insert e (tcerrors constantSolverOutput)}

instance StatusCollector Warning TypeCheckerOutput where
    w |< constantSolverOutput = constantSolverOutput{tcwarnings = Set.insert w (tcwarnings constantSolverOutput)}

data ErrorCollectorOutput = ErrorCollectorOutput
    { t :: Type,
      lrv :: Maybe LeftRightValue,
      modality :: Maybe Modality,
      posID :: Maybe (Instruction ())
    }
    deriving (Show)

data LeftRightValue = LeftValue | RightValue
    deriving (Eq, Show)

data Type
    = ErrorType
    | VoidType
    | BoolType
    | CharType
    | IntType
    | FloatType
    | StringType
    | ArrayType Bool Integer Type
    | PointerType Type
    | FunctionType [(Modality, Type)] Type
    deriving (Eq, Show)

instance PrettyPrinter Type where
    pp IntType = "int"
    pp BoolType = "bool"
    pp CharType = "char"
    pp FloatType = "float"
    pp (PointerType _) = "ref"
    pp _ = ""

instance Lattice Type where -- fix
    (/\) :: Type -> Type -> Type
    (/\) _ _ = "meet operator" `unexpectedIn` "Lattice Type (todo)"
    (\/) :: Type -> Type -> Type
    type1 \/ type2
        | type1 == type2 = type1
    IntType \/ BoolType = IntType
    BoolType \/ IntType = IntType
    IntType \/ CharType = IntType
    CharType \/ IntType = IntType
    FloatType \/ BoolType = FloatType
    BoolType \/ FloatType = FloatType
    FloatType \/ CharType = FloatType
    CharType \/ FloatType = FloatType
    FloatType \/ IntType = FloatType
    IntType \/ FloatType = FloatType
    _ \/ _ = ErrorType

instance BoundedMeetSemiLattice Type where
    top = ErrorType

---------------------------------------------------------------------------------------

-- cannot see immediately a way to use recursion schemes while preserving
-- functor over a (which can traverse the tree)
-- A separate base functor would still be incompatible with the original
-- type and would require explicit conversions. EndoFunctor it is
-- in a future life, compdata

type Block a = [Instruction a]

emapAccumBlock :: (c -> Instruction a -> (c, Instruction a)) -> (c -> Instruction a -> (c, Block a)) -> c -> Block a -> (c, Block a)
emapAccumBlock fDes fAsc acc1 instructions1 = (acc3, fold instructions3)
    where
        (acc2, instructions2) = mapAccumL fDes acc1 instructions1
        (acc3, instructions3) = mapAccumR fAsc acc2 instructions2

emapAccumLBlock :: (c -> Instruction a -> (c, Instruction a)) -> c -> Block a -> (c, Block a)
-- emapAccumLBlock f acc = emapAccumBlock f ((. List.singleton) . (,)) acc
emapAccumLBlock = mapAccumL

emapAccumRBlock :: (c -> Instruction a -> (c, Block a)) -> c -> Block a -> (c, Block a)
emapAccumRBlock f acc = emapAccumBlock (const (acc,)) f acc -- codegen visit

emapBlock :: (Instruction a -> Instruction a) -> Block a -> Block a
emapBlock = fmap

data Instruction a
    = NestedBlock Position (Block a) a
    | ConstantDecl Position Ident (Expr a) a
    | VariableDecl Position Ident (DeclType a) (Expr a) a
    | FunctionDecl Position Ident [Parameter a] (DeclType a) (Block a) a
    | Break Position a
    | Continue Position a
    | ReturnVoid Position a
    | ReturnExp Position (Expr a) a
    | While Position (Expr a) (Block a) a
    | DoWhile Position (Expr a) (Block a) a
    | For Position Ident (Expr a) (Expr a) (Expr a) (Block a) a
    | IfThen Position (Expr a) (Block a) a
    | IfThenElse Position (Expr a) (Block a) (Block a) a
    | TryCatch Position (Block a) (Block a) a
    | Throw Position String a
    | Assignment Position (Expr a) AssignmentOp (Expr a) a
    | Expression Position (Expr a) a
    deriving (Show, Functor, Foldable, Traversable)

instance EndoFunctor Instruction where
    emapAccum :: (c -> Instruction a -> (c, Instruction a)) -> (Instruction (c, a) -> Instruction (c, a)) -> c -> Instruction a -> (c, Instruction a)
    emapAccum fDes fAsc acc = cnv2 . fAsc . cnv1 . fDes acc
        where
            -- Recursion Is Cheap... But Complexity Is Expensive
            cnv1 (acc, instruction) = (acc,) <$> instruction
            cnv2 = liftA2 (,) (fst . ann) (snd <$>)

instance (StatusCollector Error a) => StatusCollector Error (Instruction a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance (StatusCollector Warning a) => StatusCollector Warning (Instruction a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance Annotated Instruction a where
    ann (NestedBlock _ _ x) = x
    ann (ConstantDecl _ _ _ x) = x
    ann (VariableDecl _ _ _ _ x) = x
    ann (FunctionDecl _ _ _ _ _ x) = x
    ann (Break _ x) = x
    ann (Continue _ x) = x
    ann (ReturnVoid _ x) = x
    ann (ReturnExp _ _ x) = x
    ann (While _ _ _ x) = x
    ann (DoWhile _ _ _ x) = x
    ann (For _ _ _ _ _ _ x) = x
    ann (IfThen _ _ _ x) = x
    ann (IfThenElse _ _ _ _ x) = x
    ann (TryCatch _ _ _ x) = x
    ann (Throw _ _ x) = x
    ann (Assignment _ _ _ _ x) = x
    ann (Expression _ _ x) = x

    updateAnn x (NestedBlock pos block _) = NestedBlock pos block x
    updateAnn x (ConstantDecl pos id expr _) = ConstantDecl pos id expr x
    updateAnn x (VariableDecl pos id declType expr _) = VariableDecl pos id declType expr x
    updateAnn x (FunctionDecl pos id params declType block _) = FunctionDecl pos id params declType block x
    updateAnn x (Break pos _) = Break pos x
    updateAnn x (Continue pos _) = Continue pos x
    updateAnn x (ReturnVoid pos _) = ReturnVoid pos x
    updateAnn x (ReturnExp pos expr _) = ReturnExp pos expr x
    updateAnn x (While pos expr block _) = While pos expr block x
    updateAnn x (DoWhile pos expr block _) = DoWhile pos expr block x
    updateAnn x (For pos id fromExpr toExpr incrExpr block _) = For pos id fromExpr toExpr incrExpr block x
    updateAnn x (IfThen pos expr block _) = IfThen pos expr block x
    updateAnn x (IfThenElse pos expr block1 block2 _) = IfThenElse pos expr block1 block2 x
    updateAnn x (TryCatch pos block1 block2 _) = TryCatch pos block1 block2 x
    updateAnn x (Throw pos msg _) = Throw pos msg x
    updateAnn x (Assignment pos expr1 op expr2 _) = Assignment pos expr1 op expr2 x
    updateAnn x (Expression pos expr _) = Expression pos expr x

instance Positioned (Instruction a) where
    position (NestedBlock pos _ _) = pos
    position (ConstantDecl pos _ _ _) = pos
    position (VariableDecl pos _ _ _ _) = pos
    position (FunctionDecl pos _ _ _ _ _) = pos
    position (Break pos _) = pos
    position (Continue pos _) = pos
    position (ReturnVoid pos _) = pos
    position (ReturnExp pos _ _) = pos
    position (While pos _ _ _) = pos
    position (DoWhile pos _ _ _) = pos
    position (For pos _ _ _ _ _ _) = pos
    position (IfThen pos _ _ _) = pos
    position (IfThenElse pos _ _ _ _) = pos
    position (TryCatch pos _ _ _) = pos
    position (Throw pos _ _) = pos
    position (Assignment pos _ _ _ _) = pos
    position (Expression pos _ _) = pos

data Parameter a = Param Position Modality Ident (DeclType a) a
    deriving (Show, Functor, Foldable, Traversable)

instance (StatusCollector Error a) => StatusCollector Error (Parameter a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance (StatusCollector Warning a) => StatusCollector Warning (Parameter a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance Annotated Parameter a where
    ann (Param _ _ _ _ x) = x
    updateAnn x (Param pos modty id declType _) = Param pos modty id declType x

instance Positioned (Parameter a) where
    position (Param pos _ _ _ _) = pos

data AssignmentOp = BasicAssignment | AssignMul | AssignAdd | AssignDiv | AssignSub | AssignPow | AssignAnd | AssignOr
    deriving (Show)

data DeclType a
    = DBoolType
    | DCharType
    | DIntType
    | DStringType
    | DFloatType
    | DVoidType
    | DArrayType Bool (Maybe (Expr a)) (DeclType a)
    | DPointerType (DeclType a)
    deriving (Show, Functor, Foldable, Traversable)

emapAccumLDeclType :: (a -> DeclType a1 -> (a, DeclType a1)) -> a -> DeclType a1 -> (a, DeclType a1)
emapAccumLDeclType f oldAcc declType = case f oldAcc declType of
    (acc, DArrayType chk expr declType) -> (newAcc, DArrayType chk expr newDeclType)
        where
            (newAcc, newDeclType) = emapAccumLDeclType f acc declType
    (acc, DPointerType declType) -> (newAcc, newDeclType)
        where
            (newAcc, newDeclType) = emapAccumLDeclType f acc declType
    leaf -> leaf

instance EndoFunctor DeclType where
    emapAccum :: (c -> DeclType a -> (c, DeclType a)) -> (DeclType (c, a) -> DeclType (c, a)) -> c -> DeclType a -> (c, DeclType a)
    emapAccum fDes fAsc acc = cnv . emapAccumR fAsc . emapAccumL fDes acc
        where
            cnv = (,) acc . (snd <$>)
            emapAccumR f declType = f $ case declType of
                (DArrayType chk expr declType) -> DArrayType chk expr (r declType)
                (DPointerType declType) -> DPointerType (r declType)
                _ -> declType
                where
                    r = emapAccumR f
            emapAccumL f acc declType = case newDeclType of
                (DArrayType chk expr declType) -> DArrayType chk (cnv <$> expr) (r declType)
                (DPointerType declType) -> DPointerType (r declType)
                _ -> cnv declType
                where
                    (newAcc, newDeclType) = f acc declType
                    r = emapAccumL f newAcc
                    cnv e = (newAcc,) <$> e

data Modality = ModalityVal | ModalityRef | ModalityRes | ModalityValRes
    deriving (Show)

instance Eq Modality where
    _ == _ = True -- ignore modality in type signature

data Expr a
    = UnaryOp Position UnaryOp (Expr a) a
    | BinaryOp Position BinaryOp (Expr a) (Expr a) a
    | Ref Position (Expr a) a
    | Deref Position (Expr a) a
    | ArrayAcc Position (Expr a) (Expr a) a
    | Id Position Ident a
    | FunctionCall Position Ident [Expr a] a
    | IntLiteral Position Integer a
    | CharLiteral Position Char a
    | StringLiteral Position String a
    | FloatLiteral Position Double a
    | BoolLiteral Position Bool a
    | ArrayLiteral Position [Expr a] a
    | CondExpr Position (Expr a) (Expr a) (Expr a) a
    deriving (Show, Functor, Foldable, Traversable)

instance EndoFunctor Expr where
    emapAccum :: (c -> Expr a -> (c, Expr a)) -> (Expr (c, a) -> Expr (c, a)) -> c -> Expr a -> (c, Expr a)
    emapAccum fDes fAsc acc = cnv . emapAccumR fAsc . emapAccumL fDes acc
        where
            cnv = liftA2 (,) (fst . ann) (snd <$>)
            emapAccumR f expr = f $ case expr of
                (UnaryOp pos unaryOp expr x) -> UnaryOp pos unaryOp (r expr) x
                (BinaryOp pos binaryOp expr1 expr2 x) -> BinaryOp pos binaryOp (r expr1) (r expr2) x
                (Ref pos expr x) -> Ref pos (r expr) x
                (Deref pos expr x) -> Deref pos (r expr) x
                (ArrayAcc pos expr1 expr2 x) -> ArrayAcc pos (r expr1) (r expr2) x
                (FunctionCall pos id exprs x) -> FunctionCall pos id (r <$> exprs) x
                (ArrayLiteral pos exprs x) -> ArrayLiteral pos (r <$> exprs) x
                (CondExpr pos expr expr1 expr2 x) -> CondExpr pos (r expr) (r expr1) (r expr2) x
                _ -> expr
                where
                    r = emapAccumR f
            emapAccumL f acc expr = case newExpr of
                (UnaryOp pos unaryOp expr x) -> UnaryOp pos unaryOp (r expr) (newAcc, x)
                (BinaryOp pos binaryOp expr1 expr2 x) -> BinaryOp pos binaryOp (r expr1) (r expr2) (newAcc, x)
                (Ref pos expr x) -> Ref pos (r expr) (newAcc, x)
                (Deref pos expr x) -> Deref pos (r expr) (newAcc, x)
                (ArrayAcc pos expr1 expr2 x) -> ArrayAcc pos (r expr1) (r expr2) (newAcc, x)
                (FunctionCall pos id exprs x) -> FunctionCall pos id (r <$> exprs) (newAcc, x)
                (ArrayLiteral pos exprs x) -> ArrayLiteral pos (r <$> exprs) (newAcc, x)
                (CondExpr pos expr expr1 expr2 x) -> CondExpr pos (r expr) (r expr1) (r expr2) (newAcc, x)
                _ -> (acc,) <$> expr
                where
                    (newAcc, newExpr) = f acc expr
                    r = emapAccumL f newAcc

instance (StatusCollector Error a) => StatusCollector Error (Expr a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance (StatusCollector Warning a) => StatusCollector Warning (Expr a) where
    (|<) e = pass1 updateAnn ((e |<) . ann)

instance Annotated Expr a where
    ann (UnaryOp _ _ _ x) = x
    ann (BinaryOp _ _ _ _ x) = x
    ann (Ref _ _ x) = x
    ann (Deref _ _ x) = x
    ann (ArrayAcc _ _ _ x) = x
    ann (Id _ _ x) = x
    ann (FunctionCall _ _ _ x) = x
    ann (IntLiteral _ _ x) = x
    ann (CharLiteral _ _ x) = x
    ann (StringLiteral _ _ x) = x
    ann (FloatLiteral _ _ x) = x
    ann (BoolLiteral _ _ x) = x
    ann (ArrayLiteral _ _ x) = x
    ann (CondExpr _ _ _ _ x) = x

    updateAnn x (UnaryOp pos op expr _) = UnaryOp pos op expr x
    updateAnn x (BinaryOp pos op expr1 expr2 _) = BinaryOp pos op expr1 expr2 x
    updateAnn x (Ref pos expr _) = Ref pos expr x
    updateAnn x (Deref pos expr _) = Deref pos expr x
    updateAnn x (ArrayAcc pos expr1 expr2 _) = ArrayAcc pos expr1 expr2 x
    updateAnn x (Id pos id _) = Id pos id x
    updateAnn x (FunctionCall pos id exprs _) = FunctionCall pos id exprs x
    updateAnn x (IntLiteral pos v _) = IntLiteral pos v x
    updateAnn x (CharLiteral pos v _) = CharLiteral pos v x
    updateAnn x (StringLiteral pos v _) = StringLiteral pos v x
    updateAnn x (FloatLiteral pos v _) = FloatLiteral pos v x
    updateAnn x (BoolLiteral pos v _) = BoolLiteral pos v x
    updateAnn x (ArrayLiteral pos exprs _) = ArrayLiteral pos exprs x
    updateAnn x (CondExpr pos expr expr1 expr2 _) = CondExpr pos expr expr1 expr2 x

instance Positioned (Expr a) where
    position (UnaryOp pos _ _ _) = pos
    position (BinaryOp pos _ _ _ _) = pos
    position (Ref pos _ _) = pos
    position (Deref pos _ _) = pos
    position (ArrayAcc pos _ _ _) = pos
    position (Id pos _ _) = pos
    position (FunctionCall pos _ _ _) = pos
    position (IntLiteral pos _ _) = pos
    position (CharLiteral pos _ _) = pos
    position (StringLiteral pos _ _) = pos
    position (FloatLiteral pos _ _) = pos
    position (BoolLiteral pos _ _) = pos
    position (ArrayLiteral pos _ _) = pos
    position (CondExpr pos _ _ _ _) = pos

data UnaryOp
    = Not
    | Neg
    | Coercion
    | PreDecr
    | PreIncr
    | PostDecr
    | PostIncr
    deriving (Eq, Show)

instance PrettyPrinter UnaryOp where
    pp Not = "!"
    pp Neg = "-"
    pp Coercion = ""
    pp PreDecr = "--"
    pp PreIncr = "++"
    pp PostDecr = "--"
    pp PostIncr = "++"

data BinaryOp
    = ArithmeticOp ArithOp
    | RelationalOp RelOp
    | BooleanOp BoolOp
    deriving (Eq, Show)

instance PrettyPrinter BinaryOp where
    pp (ArithmeticOp ar) = pp ar
    pp (RelationalOp rel) = pp rel
    pp (BooleanOp bol) = pp bol

data ArithOp = Add | Sub | Mul | Mod | Pow | Div
    deriving (Eq, Show)

instance PrettyPrinter ArithOp where
    pp Add = "+"
    pp Sub = "-"
    pp Mul = "*"
    pp Mod = "%"
    pp Pow = "^"
    pp Div = "/"

data RelOp = NotEq | GreaterThanEq | GreaterThan | LessThanEq | LessThan | Eq
    deriving (Eq, Show)

instance PrettyPrinter RelOp where
    pp NotEq = "!="
    pp GreaterThanEq = ">="
    pp GreaterThan = ">"
    pp LessThanEq = "<="
    pp LessThan = "<"
    pp Eq = "=="

data BoolOp = Or | And
    deriving (Eq, Show)

instance PrettyPrinter BoolOp where
    pp Or = "||"
    pp And = "&&"

type Ident = String

class Positioned a where
    position :: a -> Position

type Position = (Int, Int)

-- sooner or later..
astEmap :: (Instruction a -> Instruction a) -> Endo (Parameter a) -> Endo (DeclType a) -> Endo (Expr a) -> Endo (Block a)
astEmap fInstruction fParameter fDeclType fExpr = emapBlock f
    where
        f instruction = fInstruction $ case instruction of
            (NestedBlock pos block x) -> NestedBlock pos (emapBlock f block) x
            (VariableDecl pos id declType expr x) -> VariableDecl pos id (emap h declType) (emap fExpr expr) x
            (FunctionDecl pos id parameters declType block x) -> FunctionDecl pos id (g <$> parameters) (emap h declType) (emapBlock f block) x
            (ReturnExp pos expr x) -> ReturnExp pos (emap fExpr expr) x
            (While pos expr block x) -> While pos (emap fExpr expr) (emapBlock f block) x
            (DoWhile pos expr block x) -> DoWhile pos (emap fExpr expr) (emapBlock f block) x
            (For pos id fromExpr toExpr incrExpr block x) -> For pos id (emap fExpr fromExpr) (emap fExpr toExpr) (emap fExpr incrExpr) (emapBlock f block) x
            (IfThen pos expr block x) -> IfThen pos (emap fExpr expr) (emapBlock f block) x
            (IfThenElse pos expr block1 block2 x) -> IfThenElse pos (emap fExpr expr) (emapBlock f block1) (emapBlock f block2) x
            (TryCatch pos block1 block2 x) -> TryCatch pos (emapBlock f block1) (emapBlock f block2) x
            (Assignment pos expr1 op expr2 x) -> Assignment pos (emap fExpr expr1) op (emap fExpr expr2) x
            (Expression pos expr x) -> Expression pos (emap fExpr expr) x
            _ -> instruction
        g (Param pos modty id declType x) = fParameter $ Param pos modty id (emap h declType) x
        h declType = fDeclType $ case declType of
            (DArrayType chk (Just expr) declType) -> DArrayType chk (Just $ emap fExpr expr) declType
            _ -> declType
