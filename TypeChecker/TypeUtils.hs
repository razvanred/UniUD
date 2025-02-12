module TypeChecker.TypeUtils where

import AST
import Algebra.Lattice (joinLeq, (/\))
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

tree1 tree = inToStep1 <$> tree

treeOut tree = stepnToOut <$> tree

fillOutStep2 sSide sType x@(Step1{}) = Step2 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide
fillOutStep2 _ _ _ = "record" `unexpectedDuring` "fillOut2"

fillOutStep3 sSide sType sBinding x@(Step1{}) = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) sType sSide sBinding
fillOutStep3 _ _ _ _ = "record" `unexpectedDuring` "fillOut3"

newStep3 sSide sType = Step3 Set.empty Set.empty Nothing sType sSide Nothing

stepToConstantSolverOutput x =
  ConstantSolverOutput
    { cserrors = serrors x,
      cswarnings = swarnings x,
      csReplacedFromConstant = sReplacedFromConstant x
    }

inToStep1 x = Step1 (cserrors x) (cswarnings x) (csReplacedFromConstant x)

step2ToStep3 x = Step3 (serrors x) (swarnings x) (sReplacedFromConstant x) (sType x) (sSide x) Nothing

stepnToOut x' =
  TypeCheckerOutput
    { tserrors = serrors x,
      tswarnings = swarnings x,
      tcReplacedFromConstant = sReplacedFromConstant x,
      tcType = sType x,
      tcSide = sSide x,
      tcBinding = f <$> sBinding x
    }
  where
    f (depth, modty, is) = (depth, modty, void is)
    x = assertGeqStep 3 x'

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

isErrorType ErrorType = True
isErrorType _ = False

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

ePopPointer tpe e' = updateAnn x{sType = popPointer tpe, sSide = LeftValue} e
  where
    x = ann e
    e = assertEGeqStep 2 e'

ePopArray tpe e' = updateAnn x{sType = popArray tpe} e
  where
    x = ann e
    e = assertEGeqStep 2 e'

pushPointer = PointerType

ePushPointer tpe e' = updateAnn x{sType = pushPointer tpe, sSide = RightValue} e
  where
    x = ann e
    e = assertEGeqStep 2 e'

isAssignOp Not = False
isAssignOp Neg = False
isAssignOp Coercion = False
isAssignOp _ = True

unOpSup Not = BoolType
unOpSup Neg = BoolType
unOpSup op = ("operator " ++ show op) `unexpectedDuring` "unOpSup"

fixOpSup PreDecr = FloatType
fixOpSup PreIncr = FloatType
fixOpSup PostDecr = FloatType
fixOpSup PostIncr = FloatType
fixOpSup op = ("operator " ++ show op) `unexpectedDuring` "assignOpSup"

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
              _ -> ("operator " ++ show op) `unexpectedDuring` "satisfiesUnOp"
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

satisfiesAccessor indExpr expr =
  ( maybeBool (IntType /= indType) (indType, Left IntType),
    maybeBool isArray (tpe, Right "Array")
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
    sup = foldl1 (/\) exprTypes
satisfiesArrayLiteral _ = "input" `unexpectedIn` "satisfiesFCall"

satisfiesAssignment op expr1 expr2 =
  ( error1,
    maybeBool (tpe1 `joinLeq` tpe2) (tpe2, Left tpe1)
  )
  where
    error1
      | RightValue <- eSide expr1 = Just (tpe1, Right "LValue")
      | not (tpe1 `joinLeq` opSup) || ErrorType == tpe1 = Just (tpe1, Left opSup)
      | otherwise = Nothing
    opSup = assignOpSup op
    tpe1 = eType expr1
    tpe2 = eType expr2

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
