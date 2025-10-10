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

        resolveInstructions :: Block In -> ConstantTable -> ConstantTable -> Block Out
        resolveInstructions [] _ _ = []
        resolveInstructions (i : is) extEnv curEnv = case i of
            decl@(ConstantDecl _ id _ _) ->
                case curEnv !? id of
                    Nothing ->
                        resolveInstructions is extEnv (insert id (inToOut <$> decl) curEnv)
                    Just previousDecl ->
                        resolveInstructions is extEnv (insert id (ConstantAlreadyDefined (void previousDecl) |< (inToOut <$> decl)) curEnv)
            (NestedBlock pos block x) -> NestedBlock pos (resolveBlock block extEnv curEnv) (inToOut x) : rest
            (FunctionDecl pos id params declType blockIs x) ->
                FunctionDecl pos id ((fmap . (<$>)) inToOut params) (resolveDeclType declType mergedEnv) (resolveBlock blockIs extEnv curEnv) (inToOut x) : rest
            (While pos expr block x) ->
                While pos (resolveExpr expr mergedEnv) (resolveBlock block extEnv curEnv) (inToOut x) : rest
            (DoWhile pos expr block x) ->
                DoWhile pos (resolveExpr expr mergedEnv) (resolveBlock block extEnv curEnv) (inToOut x) : rest
            (For pos id fromExpr toExpr incrExpr block x) ->
                For pos id (resolveExpr fromExpr mergedEnv) (resolveExpr toExpr mergedEnv) (resolveExpr incrExpr mergedEnv) (resolveBlock block extEnv curEnv) (inToOut x) : rest
            (IfThen pos expr block x) ->
                IfThen pos (resolveExpr expr mergedEnv) (resolveBlock block extEnv curEnv) (inToOut x) : rest
            (IfThenElse pos expr block1 block2 x) ->
                IfThenElse pos (resolveExpr expr mergedEnv) (resolveBlock block1 extEnv curEnv) (resolveBlock block2 extEnv curEnv) (inToOut x) : rest
            (TryCatch pos block1 block2 x) ->
                TryCatch pos (resolveBlock block1 extEnv curEnv) (resolveBlock block2 extEnv curEnv) (inToOut x) : rest
            (VariableDecl pos id declType expr x) -> VariableDecl pos id (resolveDeclType declType mergedEnv) (resolveExpr expr mergedEnv) (inToOut x) : rest
            (Assignment pos expr1 op expr2 x) -> Assignment pos (resolveExpr expr1 mergedEnv) op (resolveExpr expr2 mergedEnv) (inToOut x) : rest
            (Expression pos expr x) -> Expression pos (resolveExpr expr mergedEnv) (inToOut x) : rest
            (ReturnExp pos expr x) -> ReturnExp pos (resolveExpr expr mergedEnv) (inToOut x) : rest
            _ -> (inToOut <$> i) : rest
            where
                mergedEnv = curEnv `union` extEnv
                rest = resolveInstructions is extEnv curEnv

        resolveDeclType :: DeclType In -> ConstantTable -> DeclType Out
        resolveDeclType (DArrayType chk (Just expr) declType) env = DArrayType chk (Just $ resolveExpr expr env) (resolveDeclType declType env)
        resolveDeclType (DPointerType declType) env = DPointerType (resolveDeclType declType env)
        resolveDeclType declType _ = inToOut <$> declType

        resolveExpr :: Expr In -> ConstantTable -> Expr Out
        resolveExpr expr env = resolve (maxDepth + 1) inToOut expr
            where
                resolve :: Int -> (In -> Out) -> Expr In -> Expr Out
                resolve depth f expr
                    | depth == 0 = (MaxRecursion |<) $ f <$> expr
                    | otherwise = case expr of
                        (UnaryOp pos op expr _) -> UnaryOp pos op (resolve depth f expr) x
                        (BinaryOp pos op expr1 expr2 _) -> BinaryOp pos op (resolve depth f expr1) (resolve depth f expr2) x
                        (Ref pos expr _) -> Ref pos (resolve depth f expr) x
                        (Deref pos expr _) -> Deref pos (resolve depth f expr) x
                        (ArrayAcc pos expr1 expr2 _) -> ArrayAcc pos (resolve depth f expr1) (resolve depth f expr2) x
                        (FunctionCall pos id exprs _) -> FunctionCall pos id (resolve depth f <$> exprs) x
                        (ArrayLiteral pos exprs _) -> ArrayLiteral pos (resolve depth f <$> exprs) x
                        (CondExpr pos expr expr1 expr2 _) -> CondExpr pos (resolve depth f expr) (resolve depth f expr1) (resolve depth f expr2) x
                        ident@(Id _ id _) -> case env !? id of
                            Just decl@(ConstantDecl _ _ expr _) -> resolve (depth - 1) (fillOutOut $ Just decl) $ {-rollback anns-} outToIn <$> expr
                            Nothing -> x <$ ident
                            _ -> "instruction" `unexpectedIn` "constanTable"
                        expr -> x <$ expr -- no recursion
                    where
                        x = f $ ann expr
