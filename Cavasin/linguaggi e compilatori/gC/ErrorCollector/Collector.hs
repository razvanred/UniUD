{-# OPTIONS_GHC -Wno-orphans #-}

module ErrorCollector.Collector (hasError, collect) where

import AST
import Data.Set (toList)
import Utils
import Prelude hiding (error)

type In = TypeCheckerOutput

hasError :: Block In -> Bool
hasError = not . all (all (null . tcerrors))

collect alsoWarns block = maybeBool (hasError block) (collectBlock block)
    where
        collectBlock = foldMap collectInstruction

        -- declTypes are not annotated, generalized anamorphism si not possible
        collectInstruction :: Instruction In -> ([String], [String])
        collectInstruction is =
            case is of
                (NestedBlock _ block _) -> err "" <> blockWE
                    where
                        blockWE = collectBlock block
                (VariableDecl _ id declType expr _) -> err ("decl " ++ id) <> declTypeWE <> exprWE
                    where
                        declTypeWE = collectDeclType declType
                        exprWE = efoldr collectExpr mempty expr
                (FunctionDecl _ id args declType block _) -> err ("fun decl " ++ id) <> argsWE <> declTypeWE <> blockWE
                    where
                        declTypeWE = collectDeclType declType
                        blockWE = collectBlock block
                        argsWE = foldMap collectArg args
                (ReturnExp _ expr _) -> err "" <> exprWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                (While _ expr block _) -> err "" <> exprWE <> blockWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                        blockWE = collectBlock block
                (DoWhile _ expr block _) -> err "" <> exprWE <> blockWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                        blockWE = collectBlock block
                (For _ _ fromExpr toExpr incrExpr block _) -> err "" <> fromExprWE <> toExprWE <> incrExprWE <> blockWE
                    where
                        fromExprWE = efoldr collectExpr mempty fromExpr
                        toExprWE = efoldr collectExpr mempty toExpr
                        incrExprWE = efoldr collectExpr mempty incrExpr
                        blockWE = collectBlock block
                (IfThen _ expr block _) -> err "" <> exprWE <> blockWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                        blockWE = collectBlock block
                (IfThenElse _ expr block1 block2 _) -> err "" <> exprWE <> block1WE <> block2WE
                    where
                        exprWE = efoldr collectExpr mempty expr
                        block1WE = collectBlock block1
                        block2WE = collectBlock block2
                (TryCatch _ block1 block2 _) -> err "" <> block1WE <> block2WE
                    where
                        block1WE = collectBlock block1
                        block2WE = collectBlock block2
                (Assignment _ expr1 op expr2 _) -> err ("op " ++ show op) <> expr1WE <> expr2WE
                    where
                        expr1WE = efoldr collectExpr mempty expr1
                        expr2WE = efoldr collectExpr mempty expr2
                (Expression _ expr _) -> err "" <> exprWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                _ -> err ""
            where
                err = makeError is (ann is)

        collectExpr :: Expr (([String], [String]), In) -> ([String], [String])
        collectExpr expr = case expr of
            (UnaryOp _ op expr _) -> err ("op " ++ show op) <> exprWE
                where
                    exprWE = fst $ ann expr
            (BinaryOp _ op expr1 expr2 _) -> err ("op " ++ show op) <> expr1WE <> expr2WE
                where
                    expr1WE = fst $ ann expr1
                    expr2WE = fst $ ann expr2
            (Ref _ expr _) -> err "" <> exprWE
                where
                    exprWE = fst $ ann expr
            (Deref _ expr _) -> err "" <> exprWE
                where
                    exprWE = fst $ ann expr
            (ArrayAcc _ expr1 expr2 _) -> err "" <> expr1WE <> expr2WE
                where
                    expr1WE = fst $ ann expr1
                    expr2WE = fst $ ann expr2
            (FunctionCall _ id exprs _) -> err ("funcall " ++ id) <> exprsWE
                where
                    exprsWE = foldMap (fst . ann) exprs
            (ArrayLiteral _ exprs _) -> err "" <> exprsWE
                where
                    exprsWE = foldMap (fst . ann) exprs
            (CondExpr _ expr expr1 expr2 _) -> err "" <> exprWE <> expr1WE <> expr2WE
                where
                    exprWE = fst $ ann expr
                    expr1WE = fst $ ann expr1
                    expr2WE = fst $ ann expr2
            _ -> err ""
            where
                err = makeError expr (snd $ ann expr)

        collectArg arg@(Param _ modty id declType _) = err ("argument " ++ show modty ++ id) <> declTypeWE
            where
                declTypeWE = collectDeclType declType
                err = makeError arg (ann arg)

        collectDeclType :: DeclType In -> ([String], [String])
        collectDeclType = fst . emapAccumLDeclType f mempty
            where
                f acc declType = (,declType) $ case declType of
                    (DArrayType _ (Just expr) _) -> acc <> efoldr collectExpr mempty expr
                    _ -> acc

        makeError pos x header = toLists header pos x

        -- toLists :: String -> Position -> (Set Error, Set Warning) -> ([String], [String])
        toLists header' e x =
            ( if null errs
                then
                    []
                else
                    header ++ (errPrefix <$> toList errs),
              if not alsoWarns || null warns
                then
                    []
                else
                    header ++ (warnPrefix <$> toList warns)
            )
            where
                header
                    | null header' = []
                    | otherwise = ["-- " ++ header' ++ ":"]
                errPrefix s = "error: at " ++ show (position e) ++ " " ++ show s
                warnPrefix s = "warning: at " ++ show (position e) ++ show s
                TypeCheckerOutput
                    { tcwarnings = warns,
                      tcerrors = errs,
                      tcReplacedFromConstant = _replacedFrom,
                      tcType = _tpe,
                      tcSide = _side,
                      tcBinding = _binding
                    } = x
