module TypeChecker.TypeUtils where

import AST
import Algebra.Lattice (joinLeq, (\/))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Set (Set, union)
import Data.Set qualified as Set
import Utils
import Prelude hiding (error, id)

type In = ConstantSolverOutput

type Out = TypeCheckerOutput

type VarTable = Map Ident (Instruction In)

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

inToStep1 x = Step1 (cserrors x) (cswarnings x) (csReplacedFromConstant x)

fillOutStep2 sSide sType x@(Step1{}) = Step2 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide
fillOutStep2 _ _ _ = "record " `unexpectedDuring` "fillOutStep2"

fillOutStep3 sSide sType sBinding x@(Step1{}) = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide sBinding
fillOutStep3 _ _ _ x = "record " ++ show x `unexpectedDuring` "fillOutStep3"

newStep3 sSide sType = Step3 Set.empty Set.empty Nothing sType sSide Nothing

stepToConstantSolverOutput x =
  ConstantSolverOutput
    { cserrors = serrors x,
      cswarnings = swarnings x,
      csReplacedFromConstant = sReplacedFromConstant x
    }

step2ToStep3 x@Step2{} = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) (sType x) (sSide x) Nothing
step2ToStep3 x = "record " ++ show x `unexpectedDuring` "step2ToStep3"

step3ToOut x@Step3{} =
  TypeCheckerOutput
    { tcerrors = serrors x,
      tcwarnings = swarnings x,
      tcReplacedFromConstant = sReplacedFromConstant x,
      tcType = sType x,
      tcSide = sSide x,
      tcBinding = f <$> sBinding x
    }
  where
    f (depth, modty, is) = (depth, modty, void is)
step3ToOut x = "record " ++ show x `unexpectedDuring` "stepnToOut"

treeStep1 :: (Functor f) => f ConstantSolverOutput -> f Step
treeStep1 tree = inToStep1 <$> tree

treeOut tree = step3ToOut <$> tree

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

-- isErrorType ErrorType = True
-- isErrorType _ = False

-- notErrorType = not . isErrorType

isLiteral (IntLiteral{}) = True
isLiteral (CharLiteral{}) = True
isLiteral (StringLiteral{}) = True
isLiteral (FloatLiteral{}) = True
isLiteral (BoolLiteral{}) = True
isLiteral _ = False

popPointer (PointerType tpe) = tpe
popPointer _ = "unexpected" `unexpectedIn` "popPointer"

popArray (ArrayType _ tpe) = tpe
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
        (not (tpe `joinLeq` opSup))
        . (,) tpe
        $ ( case op of
              Neg -> Right "numeric"
              Not -> Left opSup
              _ -> "operator " ++ show op `unexpectedDuring` "satisfiesUnOp"
          )
  where
    tpe = eType expr
    opSup = unOpSup op

satisfiesBinOp op expr1 expr2 =
  ( maybeBool (not (tpe1 `joinLeq` opSup)) (tpe1, Left expType),
    maybeBool (not (tpe2 `joinLeq` opSup)) (tpe2, Left expType)
  )
  where
    opSup = binOpSup op
    tpe1 = eType expr1
    tpe2 = eType expr2
    expType
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
    maybeBool (IntType /= indType) (indType, Left IntType)
  )
  where
    isArray = case tpe of
      (ArrayType _ _) -> True
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
        | argType == tpe ->
            if LeftValue == side
              then Nothing
              else
                Just (tpe, Right "LValue")
      _ -> Just (tpe, Left argType)
satisfiesFCall _ _ = "input" `unexpectedIn` "satisfiesFCall"

satisfiesArrayLiteral (ArrayLiteral _ exprs _)
  | null exprs = (True, False)
  | ErrorType <- sup = (False, True)
  | otherwise = (False, False)
  where
    exprTypes = eType <$> exprs
    sup = foldl1 (\/) exprTypes
satisfiesArrayLiteral _ = "input" `unexpectedIn` "satisfiesFCall"

satisfiesAssignment op expr1 expr2 =
  ( error1,
    maybeBool (tpe1 `joinLeq` tpe2 && tpe1 /= tpe2) (tpe2, Left tpe1)
  )
  where
    error1
      | RightValue <- eSide expr1 = Just (tpe1, Right "LValue")
      | not (tpe1 `joinLeq` opSup) || ErrorType == tpe1 = Just (tpe1, Left opSup)
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

-- argErrors = zipWith3 f argTypes (liftA2 (,) eSide eType <$> exprs) (argName <$> args)
--     f (modty, argType) (side, tpe) name = case modty of
--       ModalityVal | argType == tpe -> Nothing
--       ModalityRef
--         | argType == tpe ->
--             if side == LeftValue
--               then Nothing
--               else
--                 Just (name, tpe, Right "LValue")
--       _ -> Just (name, tpe, Left argType)
--     argName (Param _ id _ _) = id
