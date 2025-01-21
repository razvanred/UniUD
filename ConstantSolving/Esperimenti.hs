module ConstantSolving.Esperimenti (resolveConstants) where

import AST2
import Data.Functor.Compose
import Data.Map.Strict (union, (!?))
import Data.Map.Strict qualified as Map
import Utils

-- demo funzionante (?) di sostituzione costanti e annotazione costante origine
-- manca: collezione warning per ridefinizioni
--        massaggiamento codice per bellezza
-- su prova.hs

type ConstantTable = Map.Map Ident (Instruction ())
none = ResolveConstants {replacedFromConstant = Nothing}
replacedFrom constantDecl = ResolveConstants {replacedFromConstant = Just constantDecl}

resolveConstants instructions = resolveBlock instructions Map.empty Map.empty

resolveBlock :: Block () -> ConstantTable -> ConstantTable -> Block ASTData
resolveBlock is extEnv curEnv = resolveInstructions is (curEnv `union` extEnv) Map.empty

resolveInstructions :: [Instruction ()] -> Map.Map Ident (Instruction ()) -> Map.Map Ident (Instruction ()) -> [Instruction ASTData]
resolveInstructions [] extEnv curEnv = []
resolveInstructions (decl@(ConstantDecl pos id expr _) : is) extEnv curEnv = ConstantDecl pos id (none <$ expr) none : resolveInstructions is extEnv newEnv
  where
    newEnv = case curEnv !? id of
        Just _ -> curEnv -- warning
        Nothing -> Map.insert id decl curEnv
resolveInstructions ((NestedBlock pos blockIs _) : is) extEnv curEnv =
    NestedBlock pos (resolveBlock blockIs extEnv curEnv) none : resolveInstructions is extEnv curEnv
resolveInstructions ((FunctionDecl pos id params declType blockIs _) : is) extEnv curEnv =
    FunctionDecl pos id ((fmap . (<$)) none params) (none <$ declType) (resolveBlock blockIs extEnv curEnv) none : resolveInstructions is extEnv curEnv
resolveInstructions ((While pos expr blockIs _) : is) extEnv curEnv =
    While pos (resolveExpr expr (curEnv `union` extEnv)) (resolveBlock blockIs extEnv curEnv) none : resolveInstructions is extEnv curEnv
resolveInstructions ((IfThen pos expr blockIs _) : is) extEnv curEnv =
    IfThen pos (resolveExpr expr (curEnv `union` extEnv)) (resolveBlock blockIs extEnv curEnv) none : resolveInstructions is extEnv curEnv
resolveInstructions ((IfThenElse pos expr blockIs1 blockIs2 _) : is) extEnv curEnv =
    IfThenElse pos (resolveExpr expr (curEnv `union` extEnv)) (resolveBlock blockIs1 extEnv curEnv) (resolveBlock blockIs2 extEnv curEnv) none : resolveInstructions is extEnv curEnv
resolveInstructions (i : is) extEnv curEnv = case i of
    (VariableDecl pos id declType expr _) -> VariableDecl pos id (none <$ declType) (resolveExpr expr env) none : rest
    (Assignment pos expr1 op expr2 _) -> Assignment pos (resolveExpr expr1 env) op (resolveExpr expr2 env) none : rest
    (Expression pos expr _) -> Expression pos (resolveExpr expr env) none : rest
  where
    env = curEnv `union` extEnv
    rest = resolveInstructions is extEnv curEnv

resolveExpr :: Expr () -> ConstantTable -> Expr ASTData
resolveExpr expr env = resolve expr env none
  where
    resolve :: Expr () -> ConstantTable -> ASTData -> Expr ASTData
    resolve (UnaryOp pos op expr _) env = pass1 (UnaryOp pos op) (resolve expr env)
    resolve (BinaryOp pos op expr1 expr2 _) env = pass2 (BinaryOp pos op) (resolve expr1 env) (resolve expr2 env)
    resolve (Ref pos expr _) env = pass1 (Ref pos) (resolve expr env)
    resolve (Deref pos expr _) env = pass1 (Deref pos) (resolve expr env)
    resolve (ArrayAcc pos expr1 expr2 _) env = pass2 (ArrayAcc pos) (resolve expr1 env) (resolve expr2 env)
    resolve (FunctionCall pos id exprs _) env = \x -> FunctionCall pos id (shift resolve env x <$> exprs) x
    resolve (BasicLiteral pos basicLiteral _) env = BasicLiteral pos (none <$ basicLiteral)
    resolve (ArrayLiteral pos exprs _) env = \x -> ArrayLiteral pos (shift resolve env x <$> exprs) x
    resolve (RangedArray pos expr1 expr2 _) env = pass2 (RangedArray pos) (resolve expr1 env) (resolve expr2 env)
    resolve (Id pos id _) env = \x -> case env !? id of
        Just decl@(ConstantDecl _ _ expr _) -> resolve expr env (replacedFrom decl)
        Nothing -> Id pos id x
