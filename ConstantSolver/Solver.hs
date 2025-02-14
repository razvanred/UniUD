module ConstantSolver.Solver (solveConstants) where

import AST
import Control.Monad (void)
import Data.Map.Strict (Map, insert, union, (!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Utils
import Prelude hiding (id)

type In = ParserOutput

type Out = ConstantSolverOutput

type ConstantTable = Map Ident (Instruction Out)

fillOutOut csReplacedFromConstant x =
    ConstantSolverOutput
        { cserrors = Set.empty,
          cswarnings = pswarnings x,
          csReplacedFromConstant = csReplacedFromConstant
        }

inToOut = fillOutOut Nothing

outToIn x = ParserOutput{pswarnings = cswarnings x} -- lossless

solveConstants maxDepth instructions = resolveBlock instructions Map.empty Map.empty
    where
        resolveBlock :: Block In -> ConstantTable -> ConstantTable -> Block Out
        resolveBlock is extEnv curEnv = resolveInstructions is (curEnv `union` extEnv) Map.empty

        resolveInstructions :: [Instruction In] -> ConstantTable -> ConstantTable -> [Instruction Out]
        resolveInstructions [] _ _ = []
        resolveInstructions (i : is) extEnv curEnv = case i of
            decl@(ConstantDecl _ id _ _) ->
                case curEnv !? id of
                    Nothing ->
                        resolveInstructions is extEnv (insert id (inToOut <$> decl) curEnv)
                    Just previousDecl ->
                        resolveInstructions is extEnv (insert id (ConstantAlreadyDefined (void previousDecl) |< (inToOut <$> decl)) curEnv)
            (NestedBlock pos blockIs x) -> NestedBlock pos (resolveBlock blockIs extEnv curEnv) (inToOut x) : rest
            (FunctionDecl pos id params declType blockIs x) ->
                FunctionDecl pos id ((fmap . (<$>)) inToOut params) (resolveDeclType declType mergedEnv) (resolveBlock blockIs extEnv curEnv) (inToOut x) : rest
            (While pos expr blockIs x) ->
                While pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) (inToOut x) : rest
            (IfThen pos expr blockIs x) ->
                IfThen pos (resolveExpr expr mergedEnv) (resolveBlock blockIs extEnv curEnv) (inToOut x) : rest
            (IfThenElse pos expr blockIs1 blockIs2 x) ->
                IfThenElse pos (resolveExpr expr mergedEnv) (resolveBlock blockIs1 extEnv curEnv) (resolveBlock blockIs2 extEnv curEnv) (inToOut x) : rest
            (VariableDecl pos id declType expr x) -> VariableDecl pos id (resolveDeclType declType mergedEnv) (resolveExpr expr mergedEnv) (inToOut x) : rest
            (Assignment pos expr1 op expr2 x) -> Assignment pos (resolveExpr expr1 mergedEnv) op (resolveExpr expr2 mergedEnv) (inToOut x) : rest
            (Expression pos expr x) -> Expression pos (resolveExpr expr mergedEnv) (inToOut x) : rest
            _ -> (inToOut <$> i) : rest
            where
                mergedEnv = curEnv `union` extEnv
                rest = resolveInstructions is extEnv curEnv

        resolveDeclType :: DeclType In -> ConstantTable -> DeclType Out
        resolveDeclType (DArrayType (Just expr) declType) env = DArrayType (Just $ resolveExpr expr env) (resolveDeclType declType env)
        resolveDeclType (DPointerType declType) env = DPointerType (resolveDeclType declType env)
        resolveDeclType declType _ = inToOut <$> declType

        resolveExpr :: Expr In -> ConstantTable -> Expr Out
        resolveExpr expr env = resolve maxDepth inToOut expr
            where
                resolve :: Int -> (In -> Out) -> Expr In -> Expr Out
                resolve depth fAnn expr
                    | depth == 0 = (MaxRecursion |<) . fAnn <$> expr
                    | otherwise = case expr of
                        (UnaryOp pos op expr _) -> UnaryOp pos op (resolve depth fAnn expr) x
                        (BinaryOp pos op expr1 expr2 _) -> BinaryOp pos op (resolve depth fAnn expr1) (resolve depth fAnn expr2) x
                        (Ref pos expr _) -> Ref pos (resolve depth fAnn expr) x
                        (Deref pos expr _) -> Deref pos (resolve depth fAnn expr) x
                        (ArrayAcc pos expr1 expr2 _) -> ArrayAcc pos (resolve depth fAnn expr1) (resolve depth fAnn expr2) x
                        (FunctionCall pos id exprs _) -> FunctionCall pos id (resolve depth fAnn <$> exprs) x
                        (ArrayLiteral pos exprs _) -> ArrayLiteral pos (resolve depth fAnn <$> exprs) x
                        (RangedArray pos expr1 expr2 _) -> RangedArray pos (resolve depth fAnn expr1) (resolve depth fAnn expr2) x
                        ident@(Id _ id _) -> case env !? id of
                            Just decl@(ConstantDecl _ _ expr _) -> resolve (depth - 1) (fillOutOut $ Just decl) $ {-rollback anns-} outToIn <$> expr
                            Nothing -> x <$ ident
                            _ -> "instruction" `unexpectedIn` "constanTable"
                        expr -> x <$ expr -- no recursion
                    where
                        x = fAnn $ ann expr
