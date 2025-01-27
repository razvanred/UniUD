module TypeChecker.Algs (resolveConstantsDemo) where

import AST
import Control.Monad (void)
import Data.Map.Strict (Map, insert, union, (!?))
import Data.Map.Strict qualified as Map
import Utils
import Prelude hiding (id)

type In = ParserOutput
type Out = ConstantSolverOutput
type ConstantTable = Map Ident (Instruction In)
none = ConstantSolverOutput{csReplacedFromConstant = Nothing, csMaxRecursion = False, csConstantAlreadyDefined = Nothing}

resolveConstantsDemo maxDepth instructions = resolveBlock instructions Map.empty Map.empty
  where
    resolveBlock :: Block In -> ConstantTable -> ConstantTable -> Block Out
    resolveBlock is extEnv curEnv = resolveInstructions is (curEnv `union` extEnv) Map.empty

    resolveInstructions :: [Instruction In] -> ConstantTable -> ConstantTable -> [Instruction Out]
    resolveInstructions [] _ _ = []
    resolveInstructions (i : is) extEnv curEnv = case i of
        decl@(ConstantDecl _ id _ _) -> case curEnv !? id of
            Nothing -> (none <$ decl) : resolveInstructions is extEnv (insert id decl curEnv)
            Just previousDecl -> (none{csConstantAlreadyDefined = Just previousDecl} <$ decl) : rest -- warn constant redefined
        (NestedBlock pos blockIs _) -> NestedBlock pos (resolveBlock blockIs extEnv curEnv) none : rest
        (FunctionDecl pos id params declType blockIs _) ->
            FunctionDecl pos id ((fmap . (<$)) none params) (resolveDeclType declType mergedEnv) (resolveBlock blockIs extEnv curEnv) none : rest
        (While pos expr blockIs _) ->
            While pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) none : rest
        (IfThen pos expr blockIs _) ->
            IfThen pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) none : rest
        (IfThenElse pos expr blockIs1 blockIs2 _) ->
            IfThenElse pos (resolveExpr expr mergedEnv) (resolveBlock blockIs1 extEnv curEnv) (resolveBlock blockIs2 extEnv curEnv) none : rest
        (VariableDecl pos id declType expr _) -> VariableDecl pos id (resolveDeclType declType mergedEnv) (resolveExpr expr mergedEnv) none : rest
        (Assignment pos expr1 op expr2 _) -> Assignment pos (resolveExpr expr1 mergedEnv) op (resolveExpr expr2 mergedEnv) none : rest
        (Expression pos expr _) -> Expression pos (resolveExpr expr mergedEnv) none : rest
        _ -> (none <$ i) : rest
      where
        mergedEnv = curEnv `union` extEnv
        rest = resolveInstructions is extEnv curEnv

    resolveDeclType :: DeclType In -> ConstantTable -> DeclType Out
    resolveDeclType (ArrayType (Just expr) declType) env = ArrayType (Just $ resolveExpr expr env) (resolveDeclType declType env)
    resolveDeclType (PointerType declType) env = PointerType (resolveDeclType declType env)
    resolveDeclType declType _ = none <$ declType

    resolveExpr :: Expr In -> ConstantTable -> Expr Out
    resolveExpr expr env = resolve maxDepth expr none
      where
        resolve :: Int -> Expr In -> Out -> Expr Out
        resolve 0 expr = \x -> x{csMaxRecursion = True} <$ expr
        resolve depth (UnaryOp pos op expr _) = pass1 (UnaryOp pos op) (resolve depth expr)
        resolve depth (BinaryOp pos op expr1 expr2 _) = pass2 (BinaryOp pos op) (resolve depth expr1) (resolve depth expr2)
        resolve depth (Ref pos expr _) = pass1 (Ref pos) (resolve depth expr)
        resolve depth (Deref pos expr _) = pass1 (Deref pos) (resolve depth expr)
        resolve depth (ArrayAcc pos expr1 expr2 _) = pass2 (ArrayAcc pos) (resolve depth expr1) (resolve depth expr2)
        resolve depth (FunctionCall pos id exprs _) = pass1 (FunctionCall pos id) ((<$> exprs) . flip (resolve depth))
        resolve _ bl@(BasicLiteral{}) = (<$ bl)
        resolve depth (ArrayLiteral pos exprs _) = pass1 (ArrayLiteral pos) ((<$> exprs) . flip (resolve depth))
        resolve depth (RangedArray pos expr1 expr2 _) = pass2 (RangedArray pos) (resolve depth expr1) (resolve depth expr2)
        resolve depth ident@(Id _ id _) = \x -> case env !? id of
            Just decl@(ConstantDecl _ _ expr _) -> resolve (depth - 1) expr x{csReplacedFromConstant = Just (void decl)}
            Just _ -> "instruction" `unexpectedIn` "ConstantTable"
            Nothing -> x <$ ident
