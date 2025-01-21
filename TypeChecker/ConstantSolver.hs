module TypeChecker.ConstantSolver (resolveConstants) where

import AST
import Control.Monad (void)
import Data.Map.Strict (union, (!?))
import Data.Map.Strict qualified as Map
import Utils

type ConstantTable = Map.Map Ident (Instruction ASTData)
none = ResolveConstants {replacedFromConstant = Nothing}
replacedFrom constantDecl = ResolveConstants {replacedFromConstant = Just constantDecl}

resolveConstants maxDepth instructions = resolveBlock instructions Map.empty Map.empty
  where
    resolveBlock :: Block ASTData -> ConstantTable -> ConstantTable -> Block ASTData
    resolveBlock is extEnv curEnv = resolveInstructions is (curEnv `union` extEnv) Map.empty

    resolveInstructions :: [Instruction ASTData] -> Map.Map Ident (Instruction ASTData) -> Map.Map Ident (Instruction ASTData) -> [Instruction ASTData]
    resolveInstructions [] extEnv curEnv = []
    resolveInstructions (i : is) extEnv curEnv = case i of
        decl@(ConstantDecl pos id expr _) -> ConstantDecl pos id (none <$ expr) none : resolveInstructions is extEnv updatedEnv
          where
            updatedEnv = case curEnv !? id of
                Just _ -> curEnv -- clash warning, annotate tree
                Nothing -> Map.insert id decl curEnv
        (NestedBlock pos blockIs _) -> NestedBlock pos (resolveBlock blockIs extEnv curEnv) none : tail
        (FunctionDecl pos id params declType blockIs _) ->
            FunctionDecl pos id ((fmap . (<$)) none params) (resolveDeclType declType mergedEnv) (resolveBlock blockIs extEnv curEnv) none : tail
        (While pos expr blockIs _) ->
            While pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) none : tail
        (IfThen pos expr blockIs _) ->
            IfThen pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) none : tail
        (IfThenElse pos expr blockIs1 blockIs2 _) ->
            IfThenElse pos (resolveExpr expr mergedEnv) (resolveBlock blockIs1 extEnv curEnv) (resolveBlock blockIs2 extEnv curEnv) none : tail
        (VariableDecl pos id declType expr _) -> VariableDecl pos id (resolveDeclType declType mergedEnv) (resolveExpr expr mergedEnv) none : tail
        (Assignment pos expr1 op expr2 _) -> Assignment pos (resolveExpr expr1 mergedEnv) op (resolveExpr expr2 mergedEnv) none : tail
        (Expression pos expr _) -> Expression pos (resolveExpr expr mergedEnv) none : tail
        _ -> (none <$ i) : tail
      where
        mergedEnv = curEnv `union` extEnv
        tail = resolveInstructions is extEnv curEnv

    resolveDeclType :: DeclType ASTData -> ConstantTable -> DeclType ASTData
    resolveDeclType (ArrayType (Just expr) declType) env = ArrayType (Just $ resolveExpr expr env) (resolveDeclType declType env)
    resolveDeclType (PointerType declType) env = PointerType (resolveDeclType declType env)
    resolveDeclType declType _ = none <$ declType

    resolveExpr :: Expr ASTData -> ConstantTable -> Expr ASTData
    resolveExpr expr env = resolve maxDepth expr env none
      where
        resolve :: Int -> Expr ASTData -> ConstantTable -> ASTData -> Expr ASTData
        resolve depth (UnaryOp pos op expr _) env = pass1 (UnaryOp pos op) (resolve depth expr env)
        resolve depth (BinaryOp pos op expr1 expr2 _) env = pass2 (BinaryOp pos op) (resolve depth expr1 env) (resolve depth expr2 env)
        resolve depth (Ref pos expr _) env = pass1 (Ref pos) (resolve depth expr env)
        resolve depth (Deref pos expr _) env = pass1 (Deref pos) (resolve depth expr env)
        resolve depth (ArrayAcc pos expr1 expr2 _) env = pass2 (ArrayAcc pos) (resolve depth expr1 env) (resolve depth expr2 env)
        resolve depth (FunctionCall pos id exprs _) env = \x -> FunctionCall pos id (shift3 (resolve depth) env x <$> exprs) x
        resolve depth (BasicLiteral pos basicLiteral _) env = BasicLiteral pos (none <$ basicLiteral)
        resolve depth (ArrayLiteral pos exprs _) env = \x -> ArrayLiteral pos (shift3 (resolve depth) env x <$> exprs) x
        resolve depth (RangedArray pos expr1 expr2 _) env = pass2 (RangedArray pos) (resolve depth expr1 env) (resolve depth expr2 env)
        resolve depth ident@(Id pos id _) env
            | depth == 0 = pass $ none <$ ident
            | otherwise = \x -> case env !? id of
                Just decl@(ConstantDecl _ _ expr _) -> resolve (depth - 1) expr env (replacedFrom (void decl))
                Nothing -> Id pos id x
