module ConstantSolver.Solver {- resolveConstants -}() where

import AST
import Control.Monad (void)
import Data.Map.Strict (Map, insert, union, (!?))
import Data.Map.Strict qualified as Map
import Utils (unexpectedDuring, unexpectedIn)
import Prelude hiding (id)

type In = ParserOutput

type Out = ConstantSolverOutput

type ConstantTable = Map Ident (Instruction In)

none = ConstantSolverOutput{csReplacedFromConstant = Nothing, csMaxRecursion = False, csConstantAlreadyDefined = Nothing}

-- changeTypeParameterExpr :: b -> Expr a -> Expr b
-- changeTypeParameterExpr = (<$)

maxDepth = 5

{-
cose da ricordarsi:
ok - resolveExpr al momento della sostituzione, non salvataggio in constantTable
ok - limite ricorsione su risoluzione espressioni (contatore ad ogni chiamata di "resolveExpr")
ok - annotazione errori di clashing sul record
- maxdepth come argomento
 -}

resolveConstants :: Block In -> Block Out
resolveConstants instructionList = resolveBlock instructionList Map.empty Map.empty

resolveBlock :: Block In -> ConstantTable -> ConstantTable -> Block Out
-- resolveBlock instructionList extCt curCt = resolveInstructionList instructionList extCt (union curCt extCt) --For it's wrong!!!!
-- It should be:
resolveBlock instructionList extCt curCt = resolveInstructionList instructionList (curCt `union` extCt) Map.empty

resolveInstructionList :: [Instruction In] -> ConstantTable -> ConstantTable -> [Instruction Out]
resolveInstructionList [] _ _ = []
resolveInstructionList (i : is) extCt curCt = case i of
    decl@(ConstantDecl _ id _ _) -> case Map.lookup id curCt of
        Nothing -> (none <$ decl) : resolveInstructionList is extCt updatedCt
            where
                updatedCt = insert id (void decl) curCt
        Just previousDecl -> (none{csConstantAlreadyDefined = Just (void previousDecl)} <$ decl) : resolveInstructionList is extCt curCt
    _ -> resolveInstruction i extCt curCt : resolveInstructionList is extCt curCt

resolveInstruction :: Instruction In -> ConstantTable -> ConstantTable -> Instruction Out
resolveInstruction x extCt curCt = case x of
    NestedBlock pos blk _ -> NestedBlock pos (resolveBlock blk extCt curCt) none
    -- ConstantDecl pos id expr _ -> ConstantDecl pos id (resolveExpr expr extCt curCt) none
    VariableDecl pos id declT expr _ -> VariableDecl pos id (resolveDeclType declT extCt curCt) (resolveExpr expr extCt curCt) none
    FunctionDecl pos id parL declT blk _ -> FunctionDecl pos id (fmap (\x -> resolveParameter x extCt curCt) parL) (resolveDeclType declT extCt curCt) (resolveBlock blk extCt curCt) none
    Break pos _ -> Break pos none
    Continue pos _ -> Continue pos none
    ReturnVoid pos _ -> ReturnVoid pos none
    ReturnExp pos expr _ -> ReturnExp pos (resolveExpr expr extCt curCt) none
    While pos expr blk _ -> While pos (resolveExpr expr extCt curCt) (resolveBlock blk extCt curCt) none
    IfThen pos expr blk _ -> IfThen pos (resolveExpr expr extCt curCt) (resolveBlock blk extCt curCt) none
    IfThenElse pos expr1 blk1 blk2 _ -> IfThenElse pos (resolveExpr expr1 extCt curCt) (resolveBlock blk1 extCt curCt) (resolveBlock blk2 extCt curCt) none
    Assignment pos expr1 aop expr2 _ -> Assignment pos (resolveExpr expr1 extCt curCt) aop (resolveExpr expr2 extCt curCt) none
    Expression pos expr _ -> Expression pos (resolveExpr expr extCt curCt) none
    ConstantDecl{} -> "instruction" `unexpectedDuring` "resolveInstruction"

resolveParameter :: Parameter In -> ConstantTable -> ConstantTable -> Parameter Out
resolveParameter (Param mod id declT _) extCt curCt = Param mod id (resolveDeclType declT extCt curCt) none

resolveDeclType :: DeclType In -> ConstantTable -> ConstantTable -> DeclType Out
resolveDeclType x extCt curCt = case x of
    ArrayType expr declT -> ArrayType (fmap (\x -> resolveExpr x extCt curCt) expr) (resolveDeclType declT extCt curCt)
    PointerType declT -> PointerType (resolveDeclType declT extCt curCt)
    _ -> fmap (const none) x

resolveExpr :: Expr In -> ConstantTable -> ConstantTable -> Expr Out
resolveExpr x extCt curCt = resolveExprDepth maxDepth x extCt curCt none

resolveExprDepth :: Int -> Expr In -> ConstantTable -> ConstantTable -> Out -> Expr Out
resolveExprDepth 0 expr _ _ annotation = annotation{csMaxRecursion = True} <$ expr
resolveExprDepth maxDepth (UnaryOp pos unop expr _) extCt curCt annotation = UnaryOp pos unop (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (BinaryOp pos bop expr1 expr2 _) extCt curCt annotation = BinaryOp pos bop (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth maxDepth (Ref pos expr _) extCt curCt annotation = Ref pos (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (Deref pos expr _) extCt curCt annotation = Deref pos (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (ArrayAcc pos expr1 expr2 _) extCt curCt annotation = ArrayAcc pos (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth maxDepth (FunctionCall pos id actualParameters _) extCt curCt annotation = FunctionCall pos id (map (\y -> resolveExprDepth maxDepth y extCt curCt annotation) actualParameters) annotation
resolveExprDepth maxDepth (ArrayLiteral pos listExp _) extCt curCt annotation = ArrayLiteral pos (map (\y -> resolveExprDepth maxDepth y extCt curCt annotation) listExp) annotation
resolveExprDepth maxDepth (RangedArray pos expr1 expr2 _) extCt curCt annotation = RangedArray pos (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth _ bl@(BasicLiteral{}) _ _ annotation = annotation <$ bl
resolveExprDepth maxDepth ident@(Id _ id _) extCt curCt annotation = case union curCt extCt !? id of
    Nothing -> annotation <$ ident
    Just constDecl@(ConstantDecl _ _ expr _) -> resolveExprDepth (maxDepth - 1) expr extCt curCt annotation{csReplacedFromConstant = Just (void constDecl)}
    Just _ -> "instruction" `unexpectedIn` "ConstantTable"
