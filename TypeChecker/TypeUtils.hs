{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid partial function" #-}
module TypeChecker.TypeUtils where

import AST
import Algebra.Lattice (joinLeq, (\/))
import Control.Applicative ((<|>))
import Data.Set (Set, union)
import Data.Set qualified as Set
import Utils
import Prelude hiding (error, id)

data Step
  = Step1
      { serrors :: Set Error,
        swarnings :: Set Warning,
        sReplacedFromConstant :: Maybe (Instruction ConstantSolverOutput)
      }
  | Step2
      { serrors :: Set Error,
        swarnings :: Set Warning,
        sReplacedFromConstant :: Maybe (Instruction ConstantSolverOutput),
        sType :: Type,
        sSide :: LeftRightValue
      }
  | Step3
      { serrors :: Set Error,
        swarnings :: Set Warning,
        sReplacedFromConstant :: Maybe (Instruction ConstantSolverOutput),
        sType :: Type,
        sSide :: LeftRightValue,
        sBinding :: Maybe (Int, Modality, Instruction Step)
      }
  deriving (Show)

instance StatusCollector Error Step where
  e |< step = step{serrors = Set.insert e (serrors step)}

instance StatusCollector Warning Step where
  w |< step = step{swarnings = Set.insert w (swarnings step)}

fillOutStep2 sSide sType x@(Step1{}) = Step2 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide
fillOutStep2 _ _ _ = "record " `unexpectedDuring` "fillOutStep2"

fillOutStep3 sSide sType sBinding x@(Step1{}) = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide sBinding
fillOutStep3 _ _ _ x = "record " ++ show x `unexpectedDuring` "fillOutStep3"

newStep3 sSide sType = Step3 Set.empty Set.empty Nothing sType sSide Nothing

step2ToStep3 x@Step2{} = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) (sType x) (sSide x) Nothing
step2ToStep3 x = "record " ++ show x `unexpectedDuring` "step2ToStep3"

x |<> oldX = x{serrors = serrors x `union` serrors oldX, swarnings = swarnings x `union` swarnings oldX}

infixl 9 |<>

assertEGeqStep :: (Annotated a2 Step) => Int -> a2 Step -> a2 Step
assertEGeqStep num e = e
  where
    !_ = assertGeqStep num $ ann e

assertGeqStep :: Int -> Step -> Step
assertGeqStep num x
  | step >= num = x
  | otherwise = error $ "uh oh! assertGeqStep failed, expected " ++ show num ++ ", was " ++ show step
  where
    step = case x of
      Step1{} -> 1
      Step2{} -> 2
      Step3{} -> 3

eType e = sType $ ann (assertEGeqStep 2 e)

eSide e = sSide $ ann (assertEGeqStep 2 e)

joinLe tpe1 tpe2 = tpe1 `joinLeq` tpe2 && tpe1 /= tpe2

notJoinLeq tpe1 tpe2 = not (tpe1 `joinLeq` tpe2)

isLiteral (IntLiteral{}) = True
isLiteral (CharLiteral{}) = True
isLiteral (StringLiteral{}) = True
isLiteral (FloatLiteral{}) = True
isLiteral (BoolLiteral{}) = True
isLiteral _ = False

isCompLiteral (ArrayLiteral{}) = True
isCompLiteral (StringLiteral{}) = True
isCompLiteral _ = False

popPointer (PointerType tpe) = tpe
popPointer _ = "unexpected" `unexpectedIn` "popPointer"

popArray (ArrayType _ _ tpe) = tpe
popArray _ = "unexpected" `unexpectedIn` "popArray"

pushPointer = PointerType

isAssignOp Not = False
isAssignOp Neg = False
isAssignOp Coercion = False
isAssignOp _ = True

unOpSup Not = BoolType
unOpSup Neg = BoolType
unOpSup PreDecr = FloatType
unOpSup PreIncr = FloatType
unOpSup PostDecr = FloatType
unOpSup PostIncr = FloatType
unOpSup op = "operator " ++ show op `unexpectedDuring` "unOpSup"

binOpSup (ArithmeticOp Add) = FloatType
binOpSup (ArithmeticOp Sub) = FloatType
binOpSup (ArithmeticOp Mul) = FloatType
binOpSup (ArithmeticOp Mod) = IntType
binOpSup (ArithmeticOp Pow) = FloatType
binOpSup (ArithmeticOp Div) = FloatType
binOpSup (RelationalOp _) = FloatType
binOpSup (BooleanOp _) = BoolType

assignOpSup BasicAssignment = ErrorType
assignOpSup _ = FloatType

satisfiesUnOp op expr
  | isAssignOp op, RightValue <- eSide expr = Just (eType expr, Right "LValue")
  | otherwise =
      maybeBool
        (tpe `notJoinLeq` opSup)
        . (,) tpe
        $ ( case op of
              Neg -> Right "numeric"
              Not -> Left opSup
              _ -> "operator " ++ show op `unexpectedDuring` "satisfiesUnOp"
          )
  where
    tpe = eType expr
    opSup = unOpSup op

satisfiesBinOp op expr1 expr2
  | ArithmeticOp _ <- op,
    BoolType <- tpe1 \/ tpe2 =
      ( Just (tpe1, Right "Numeric"),
        Just (tpe2, Right "Numeric")
      )
  | otherwise =
      let
        opSup
          | RelationalOp op <- op, (Eq == op || NotEq == op) && (tpe1 \/ tpe2 == StringType) = StringType
          | otherwise = binOpSup op
      in
        ( maybeBool (tpe1 `notJoinLeq` opSup) (tpe1, Left $ expType opSup),
          maybeBool (tpe2 `notJoinLeq` opSup) (tpe2, Left $ expType opSup)
        )
  where
    tpe1 = eType expr1
    tpe2 = eType expr2
    expType opSup
      | tpe1 `joinLeq` opSup = tpe1
      | tpe2 `joinLeq` opSup = tpe2
      | otherwise = opSup

satisfiesRef expr
  | ErrorType /= tpe,
    LeftValue <- eSide expr =
      Nothing
  | otherwise = Just $ case tpe of
      (PointerType _) -> (tpe, Right "LValue")
      _ -> (tpe, Right "Pointer")
  where
    tpe = eType expr

satisfiesDeref expr
  | PointerType _ <- tpe =
      Nothing
  | otherwise = Just (tpe, Right "Pointer")
  where
    tpe = eType expr

satisfiesAccessor expr indExpr =
  ( maybeBool (not isArray) (tpe, Right "Array"),
    maybeBool (indType `notJoinLeq` IntType) (indType, Left IntType)
  )
  where
    isArray = case tpe of
      (ArrayType{}) -> True
      _ -> False
    indType = eType indExpr
    tpe = eType expr

satisfiesFCall (FunctionType argTypes _) exprs =
  case (argCount, foldl (<|>) Nothing argErrors) of
    (False, Nothing) -> (False, argErrors)
    _ -> (argCount, argErrors)
  where
    argCount = length argTypes /= length exprs
    argErrors = zipWith f argTypes (liftA2 (,) eSide eType <$> exprs)
    f (modty, argType) (side, tpe) = case modty of
      ModalityVal | tpe `joinLeq` argType -> Nothing
      ModalityRef
        | tpe `joinLeq` argType ->
            if LeftValue == side
              then Nothing
              else
                Just (tpe, Right "LValue")
      ModalityRes -- todo, needs more
        | tpe `joinLeq` argType ->
            if LeftValue == side
              then Nothing
              else
                Just (tpe, Right "LValue")
      ModalityValRes -- todo, needs more
        | tpe `joinLeq` argType ->
            if LeftValue == side
              then Nothing
              else
                Just (tpe, Right "LValue")
      _ -> Just (tpe, Left argType)
satisfiesFCall _ _ = "input" `unexpectedIn` "satisfiesFCall"

satisfiesCondExpr expr expr1 expr2
  | ErrorType == tpe1 || ErrorType == tpe2 = (2 :: Int, condErr)
  | ErrorType <- sup = (1 :: Int, condErr)
  | otherwise = (0 :: Int, condErr)
  where
    condErr
      | tpe `joinLeq` BoolType = Nothing
      | otherwise = Just (tpe, Left BoolType)
    sup = tpe1 \/ tpe2
    tpe = eType expr
    tpe1 = eType expr1
    tpe2 = eType expr2

satisfiesArrayLiteral (ArrayLiteral _ exprs _)
  | null exprs = (True, False)
  | ErrorType `elem` exprTypes = (True, True)
  | ErrorType <- sup = (False, True)
  | otherwise = (False, False)
  where
    exprTypes = eType <$> exprs
    sup = foldl1 (\/) exprTypes
satisfiesArrayLiteral _ = "input" `unexpectedIn` "satisfiesFCall"

-- non expressions

satisfiesAssignment op expr1 expr2 =
  ( error1,
    maybeBool (tpe2 `notJoinLeq` tpe1) (tpe2, Left tpe1)
  )
  where
    error1
      | RightValue <- eSide expr1 = Just (tpe1, Right "LValue")
      | tpe1 `notJoinLeq` opSup || ErrorType == tpe1 = Just (tpe1, Left opSup)
      | otherwise = Nothing
    opSup = assignOpSup op
    tpe1 = eType expr1
    tpe2 = eType expr2

satisfiesTypeExpr expr =
  if isLiteral expr
    then
      if tpe `joinLeq` IntType
        then case litVal expr of
          len | len >= 1 -> Nothing -- TAC team said ok
          _ -> Just $ TypeMismatch tpe (Right "Positive value")
        else
          Just $ TypeMismatch tpe (Right "Integral")
    else Just NonConstExpr
  where
    tpe = eType expr
    litVal (IntLiteral _ v _) = v
    litVal (CharLiteral _ v _) = fromIntegral $ fromEnum v
    litVal _ = "literal" `unexpectedDuring` "litVal"
