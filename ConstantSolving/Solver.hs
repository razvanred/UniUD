module ConstantSolving.Solver (resolveConstants) where

import AST
import Data.Map.Strict (union, (!?))
import Data.Map.Strict qualified as Map

-- type ConstantTable = Map.Map Ident (Expr ASTData)
type ConstantTable = Map.Map Ident (Instruction ())

none = ResolveConstants {replacedFromConstant = Nothing}
replacedFrom constantDecl = ResolveConstants {replacedFromConstant = Just constantDecl}
maxDepth = 5

{-
cose da ricordarsi:
ok - resolveExpr al momento della sostituzione, non salvataggio in constantTable
ok - limite ricorsione su risoluzione espressioni (contatore ad ogni chiamata di "resolveExpr")
- annotazione errori di clashing sul record
 -}

resolveConstants :: Block ASTData -> Block ASTData
resolveConstants instructionList = resolveBlock instructionList Map.empty Map.empty

resolveBlock :: Block ASTData -> ConstantTable -> ConstantTable -> Block ASTData
resolveBlock instructionList extCt curCt = resolveInstructionList instructionList extCt (union curCt extCt)

resolveInstructionList :: [Instruction ASTData] -> ConstantTable -> ConstantTable -> [Instruction ASTData]
resolveInstructionList [] _ _ = []
resolveInstructionList (x : xs) extCt curCt = case x of
    decl@(ConstantDecl _ id expr _) -> resolveInstruction x extCt curCt : resolveInstructionList xs extCt (updateCT decl curCt)
    _ -> resolveInstruction x extCt curCt : resolveInstructionList xs extCt curCt

updateCT :: Instruction ASTData -> ConstantTable -> ConstantTable
updateCT (ConstantDecl pos id exp a) curtab = case Map.lookup id curtab of
    Nothing -> Map.insert id (ConstantDecl pos id (turnExpr exp ()) ()) curtab

-- Just _ -> error "Una costante non può essere nuovamente dichiarata nello stesso scope." -- qui farà qualcosa di meglio

resolveInstruction :: Instruction ASTData -> ConstantTable -> ConstantTable -> Instruction ASTData
resolveInstruction x extCt curCt = case x of
    NestedBlock pos blk _ -> NestedBlock pos (resolveBlock blk extCt curCt) none
    ConstantDecl pos id expr _ -> ConstantDecl pos id (resolveExpr expr extCt curCt) none
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

resolveParameter :: Parameter ASTData -> ConstantTable -> ConstantTable -> Parameter ASTData
resolveParameter (Param mod id declT _) extCt curCt = Param mod id (resolveDeclType declT extCt curCt) none

resolveDeclType :: DeclType ASTData -> ConstantTable -> ConstantTable -> DeclType ASTData
resolveDeclType x extCt curCt = case x of
    ArrayType expr declT -> ArrayType (fmap (\x -> resolveExpr x extCt curCt) expr) (resolveDeclType declT extCt curCt)
    PointerType declT -> PointerType (resolveDeclType declT extCt curCt)
    _ -> fmap (const none) x

resolveExpr :: Expr ASTData -> ConstantTable -> ConstantTable -> Expr ASTData
resolveExpr x extCt curCt = resolveExprDepth maxDepth x extCt curCt none

resolveExprDepth :: Int -> Expr ASTData -> ConstantTable -> ConstantTable -> ASTData -> Expr ASTData
resolveExprDepth 0 x _ _ _ = turnExpr x none
resolveExprDepth maxDepth (UnaryOp pos unop expr _) extCt curCt annotation = UnaryOp pos unop (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (BinaryOp pos bop expr1 expr2 _) extCt curCt annotation = BinaryOp pos bop (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth maxDepth (Ref pos expr _) extCt curCt annotation = Ref pos (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (Deref pos expr _) extCt curCt annotation = Deref pos (resolveExprDepth maxDepth expr extCt curCt annotation) annotation
resolveExprDepth maxDepth (ArrayAcc pos expr1 expr2 _) extCt curCt annotation = ArrayAcc pos (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth maxDepth (FunctionCall pos id actualParameters _) extCt curCt annotation = FunctionCall pos id (map (\y -> resolveExprDepth maxDepth y extCt curCt annotation) actualParameters) annotation
resolveExprDepth maxDepth (ArrayLiteral pos listExp _) extCt curCt annotation = ArrayLiteral pos (map (\y -> resolveExprDepth maxDepth y extCt curCt annotation) listExp) annotation
resolveExprDepth maxDepth (RangedArray pos expr1 expr2 _) extCt curCt annotation = RangedArray pos (resolveExprDepth maxDepth expr1 extCt curCt annotation) (resolveExprDepth maxDepth expr2 extCt curCt annotation) annotation
resolveExprDepth maxDepth (BasicLiteral pos bsl _) extCt curCt annotation = BasicLiteral pos (fmap (const none) bsl) annotation
resolveExprDepth maxDepth (Id pos id _) extCt curCt annotation = case Map.lookup id curCt of
    Nothing -> case Map.lookup id extCt of
        Nothing -> Id pos id none
        Just constDecl@(ConstantDecl pos2 id2 expr ast) -> resolveExprDepth (maxDepth - 1) (turnExpr expr (Parse ())) extCt curCt (replacedFrom constDecl)
    -- Just _ -> ERROR
    Just constDecl@(ConstantDecl pos2 id2 expr ast) -> resolveExprDepth (maxDepth - 1) (turnExpr expr (Parse ())) extCt curCt (replacedFrom constDecl)

turnExpr :: Expr a -> b -> Expr b
turnExpr (UnaryOp pos unop expr _) t = UnaryOp pos unop (turnExpr expr t) t
turnExpr (BinaryOp pos bop expr1 expr2 _) t = BinaryOp pos bop (turnExpr expr1 t) (turnExpr expr2 t) t
turnExpr (Ref pos expr _) t = Ref pos (turnExpr expr t) t
turnExpr (Deref pos expr _) t = Deref pos (turnExpr expr t) t
turnExpr (ArrayAcc pos expr1 expr2 _) t = ArrayAcc pos (turnExpr expr1 t) (turnExpr expr2 t) t
turnExpr (FunctionCall pos id actualParameters _) t = FunctionCall pos id (map (\y -> turnExpr y t) actualParameters) t
turnExpr (ArrayLiteral pos listExp _) t = ArrayLiteral pos (map (\y -> turnExpr y t) listExp) t
turnExpr (RangedArray pos expr1 expr2 _) t = RangedArray pos (turnExpr expr1 t) (turnExpr expr2 t) t
turnExpr (BasicLiteral pos bsl _) t = BasicLiteral pos (fmap (\x -> t) bsl) t
turnExpr (Id pos id _) t = Id pos id t
