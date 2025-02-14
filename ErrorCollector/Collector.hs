{-# OPTIONS_GHC -Wno-orphans #-}

module ErrorCollector.Collector (collectErrors) where

import AST
import Data.Set (Set, toList)
import Data.Set qualified as Set
import Utils
import Prelude hiding (error)

type In = TypeCheckerOutput

sfilter = Set.filter

hasError :: Block In -> Bool
hasError block = not (all (all (null . tcerrors)) block)

collectErrors alsoWarns block = maybeBool (hasError block) (collectBlock block)
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
                (FunctionDecl pos id args declType block _) -> err ("fun decl " ++ id) <> argsWE <> declTypeWE <> blockWE
                    where
                        declTypeWE = collectDeclType declType
                        blockWE = collectBlock block
                        argsWE = foldMap (collectArg pos) args
                (ReturnExp _ expr _) -> err "" <> exprWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                (While _ expr block _) -> err "" <> exprWE <> blockWE
                    where
                        exprWE = efoldr collectExpr mempty expr
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
                (Assignment _ expr1 op expr2 _) -> err ("op " ++ show op) <> expr1WE <> expr2WE
                    where
                        expr1WE = efoldr collectExpr mempty expr1
                        expr2WE = efoldr collectExpr mempty expr2
                (Expression _ expr _) -> err "" <> exprWE
                    where
                        exprWE = efoldr collectExpr mempty expr
                _ -> err ""
            where
                err = makeError (position is) (ann is)

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
            (RangedArray _ expr1 expr2 _) -> err "" <> expr1WE <> expr2WE
                where
                    expr1WE = fst $ ann expr1
                    expr2WE = fst $ ann expr2
            _ -> err ""
            where
                err = makeError (position expr) (snd $ ann expr)

        collectArg pos arg@(Param modty id declType _) = err ("argument " ++ show modty ++ id) <> declTypeWE
            where
                declTypeWE = collectDeclType declType
                err = makeError pos (ann arg)

        collectDeclType :: DeclType In -> ([String], [String])
        collectDeclType = fst . emapAccumLDeclType f mempty
            where
                f acc declType = (,declType) $ case declType of
                    (DArrayType (Just expr) _) -> acc <> efoldr collectExpr mempty expr
                    _ -> acc

        makeError pos x header = toLists header pos (errs, warns)
            where
                TypeCheckerOutput
                    { tcwarnings = warns,
                      tcerrors = errs,
                      tcType = _tpe,
                      tcSide = _side,
                      tcBinding = _binding
                    } = x

        toLists :: String -> Position -> (Set Error, Set Warning) -> ([String], [String])
        toLists header' pos (errs, warns) =
            ( if null newErrs
                then
                    []
                else
                    header ++ (errPrefix <$> toList newErrs),
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
                errPrefix s = "error: at " ++ show pos ++ " " ++ show s
                warnPrefix s = "warning: at " ++ show pos ++ show s
                newErrs = sfilter f errs
                f err = case err of -- hackish
                    (TypeMismatch _ (Left ErrorType)) -> False
                    _ -> True
