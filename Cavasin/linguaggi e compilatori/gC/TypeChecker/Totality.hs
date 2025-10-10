{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module TypeChecker.Totality (checkTotality) where

import AST
import Data.Foldable (fold)
import Data.Traversable (mapAccumL)
import TypeChecker.TypeUtils
import Utils
import Prelude hiding (error)

data Status = Status
    { total :: Bool,
      annUnreachable :: Bool
    }

-- totality

-- checkTotality :: Block Step -> Block Step
checkTotality = snd . checkFBlock (0, 0) VoidType Status{total = False, annUnreachable = False}

checkFBlock :: Position -> Type -> Status -> Block Step -> (Status, Block Step)
checkFBlock pos tpe status' block1 = (status, block3)
    where
        (status@Status{total}, block2) = fold <$> mapAccumL checkInstruction status' block1
        block3
            | not total,
              VoidType <- tpe =
                block2 ++ [ReturnVoid pos (newStep3 RightValue VoidType)]
            | otherwise = block2

checkBlock = ((fold <$>) .) . mapAccumL checkInstruction

checkInstruction :: Status -> Instruction Step -> (Status, [Instruction Step])
checkInstruction status (FunctionDecl pos id params declType block' x) =
    (status, [fDecl])
    where
        (FunctionType _ retType) = sType x
        (Status{total}, block) = checkFBlock pos retType Status{total = False, annUnreachable = False} block'
        fDecl = case FunctionDecl pos id params declType block x of
            t
                | not total,
                  VoidType /= retType ->
                    NonTotalFunction |< t
            t -> t
checkInstruction status@Status{annUnreachable = True} is =
    (status{annUnreachable = False}, [UnreachableCode |< is])
checkInstruction status@Status{total = False} is
    | IfThenElse pos expr block1' block2' x <- is =
        let
            (Status{total = block1Tot}, block1) = checkBlock status block1'
            (Status{total = block2Tot}, block2) = checkBlock status block2'
        in
            (f block1Tot block2Tot, [IfThenElse pos expr block1 block2 x])
    | TryCatch pos block1' block2' x <- is =
        let
            (Status{total = block1Tot}, block1) = checkBlock status block1'
            (Status{total = block2Tot}, block2) = checkBlock status block2'
        in
            (f block1Tot block2Tot, [TryCatch pos block1 block2 x])
    where
        f block1Tot block2Tot = Status{total, annUnreachable}
            where
                total = block1Tot && block2Tot
                annUnreachable
                    | total = True
                    | otherwise = False
checkInstruction status@Status{total = False} (NestedBlock pos block' x) =
    (status{total, annUnreachable}, [NestedBlock pos block x])
    where
        (Status{total}, block) = checkBlock status block'
        annUnreachable
            | total = True
            | otherwise = False
checkInstruction Status{total = False} is'@(ReturnExp pos expr x) =
    (Status{total = True, annUnreachable = True}, is)
    where
        is = case eType expr of
            VoidType -> [Expression pos expr x, ReturnVoid pos x]
            _ -> [is']
checkInstruction Status{total = False} is@(ReturnVoid{}) =
    (Status{total = True, annUnreachable = True}, [is])
checkInstruction status is = (status, [is])
