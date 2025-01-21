module ConstantSolving.Solver where

import AST

-- import Parser.OutputAST qualified

import Data.Map.Strict qualified as Map

-- trovo una dichiarazione di costante
-- la lascio scritta nel punto dov'è e me la copio nella tabella
-- vado avanti in tutto l'albero e faccio le sostituzioni

-- MARCO 2025-01-25
-- secondo me dobbiamo chiarire meglio cosa mettere dentro la ConstantTable
-- se usiamo come chiave id e come valore la dichiarazione mi pare vada bene
-- ma allora non mettiamo Expr di ASTData come valore
-- Expr ASTData lo useremo invece nella struttura dati che diamo un output
-- cioè invece di questo:
-- type ConstantTable = Map.Map Ident (Expr ASTData)
-- mettiamo questo:
type ConstantTable a = Map.Map Ident (Declaration a)

-- però ora ho tenuto il parametro di tipo, perché secondo me se si parte con tipi polimorfi
-- bisogna tenersi il polimorfismo e semmai istanziare in seconda battuta con () se non ci frega
-- così possiamo scrivere codice sempre polimorfo (almeno così ora mi viene da pensare...)
-- secondo me anche su ASTData bisogna fare una ulteriore riflessione

-- in seguito eviterei di usare il termine "env" perché ricorda l'environment mentre qui è solo una tabella di costanti, potremmo usare "ct"
-- inoltre, forse è questo il momento per aggiornare il currEnv con l'extEnv?
-- cioè nella chiamata a resolveInstructionList in modo tale che partiamo con un Env che si è preso tutte le cose esterne
resolveConstants :: Block ASTData -> ConstantTable a -> ConstantTable a -> Block ASTData
resolveConstants (Block p instructions ast) extEnv curEnv = Block p (resolveInstructionList instructions extEnv curEnv) (ResolveConstants Nothing)

resolveInstructionList :: [Instruction ASTData] -> ConstantTable a -> ConstantTable a -> [Instruction ASTData]
resolveInstructionList [] _ _ = []
resolveInstructionList (x : xs) extEnv curEnv = case x of
    Declaration (ConstantDecl _ id expr _) ast -> (resolveInstruction x extEnv curEnv) : resolveInstructionList xs extEnv (Map.insert id expr curEnv) -- qui va updateCT
    Declaration (VariableDecl pos id decltype expr _) ast -> (resolveInstruction x extEnv curEnv) : resolveInstructionList xs extEnv curEnv
    Declaration (FunctionDecl pos id list decltype blk _) ast -> (resolveInstruction x extEnv curEnv) : resolveInstructionList xs extEnv curEnv
    _ -> (resolveInstruction x extEnv curEnv) : resolveInstructionList xs extEnv curEnv

-- MARCO 2025-01-25
-- inserisce nella tabella id e relativa dichiarazione, torna errore se id già presente nella tabella
updateCT :: Declaration a -> ConstantTable a -> ConstantTable a
updateCT decl@(ConstantDecl _ id _ _) ct = case Map.lookup id ct of
    Nothing -> Map.insert id decl ct
    Just _ -> error "Una costante non può essere nuovamente dichiarata nello stesso scope." -- qui farà qualcosa di meglio

-- MARCO 2025-01-25
-- cerca a cosa corrisponde Ident (cerca nella constant table corrente perché dovrebbe essere già aggiornata quando si è entrati nel blocco, no?)
lookupCT :: Expr a -> ConstantTable a -> Expr ASTData
lookupCT (Id pos id a) ct = case Map.lookup id ct of
    Nothing -> Id pos id (ResolveConstants Nothing)
    Just (ConstantDecl pos2 id2 expr2 a) -> Id pos id (ResolveConstants (Just (ConstantDecl pos2 id2 expr2 ()))) -- però qui ricasca l'asino (io in questo caso)
    -- si potrebbe risolvere con una funzione che trasforma Expr di un certo tipo in Expr di un altro (cosa che ci può servire anche in futuro)

resolveInstruction :: Instruction ASTData -> ConstantTable a -> ConstantTable a -> Instruction ASTData
resolveInstruction (Statement statement ast) extEnv curEnv = Statement (resolveStatement statement extEnv curEnv) (ResolveConstants Nothing)
resolveInstruction (Declaration declaration ast) extEnv curEnv = Declaration (resolveDeclaration declaration extEnv curEnv) (ResolveConstants Nothing)

-- resolveStatement :: Statement ASTData -> ConstantTable -> ConstantTable -> Statement ASTData
-- resolveStatement x extEnv curEnv = case x of
--    NestedBlock blk ast -> resolveConstants blk extEnv curEnv
--    ReturnExp pos expr ast -> (ResolveConstants Nothing)
--    While pos expr blk ast -> (ResolveConstants Nothing)
--    IfThen pos expr blk ast -> (ResolveConstants Nothing)
--    IfThenElse pos expr blk1 blk2 ast -> IfThenElse pos expr blk1 (ResolveConstants Nothing)
--    Assignment pos expr1 assOp expr2 ast -> Assignment pos expr assOp expr (ResolveConstants Nothing)
--    Expression pos expr ast -> Expression pos resolve expr (ResolveConstants Nothing)
--    ReturnVoid pos ast -> (ResolveConstants Nothing)
--    Break pos ast -> (ResolveConstants Nothing)
--    Continue pos ast -> (ResolveConstants Nothing)

resolveExpr :: Expr ASTData -> ConstantTable a -> ConstantTable a -> Expr ASTData
resolveExpr (UnaryOp pos unop expr _) extEnv curEnv = UnaryOp pos (resolveOp unop) (resolveExpr expr) (ResolveConstants Nothing)
resolveExpr (Id pos id _) extEnv curEnv = Id pos id
