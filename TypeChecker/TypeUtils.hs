module TypeChecker.TypeUtils where

import AST
import Algebra.Lattice (joinLeq, (\/))
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
        sBinding :: Maybe (Int, Instruction Step)
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
      tcSide = sSide x
    }
  where
    x = assertGeqStep 2 x'

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

pushPointer = PointerType

ePopPointer e' = updateAnn x{sType = (popPointer . sType) x} e
  where
    x = ann e
    e = assertEGeqStep 2 e'

ePushPointer e' = updateAnn x{sType = (pushPointer . sType) x} e
  where
    x = ann e
    e = assertEGeqStep 2 e'

unOpSup Not = BoolType
unOpSup Neg = BoolType
unOpSup op = ("operator " ++ show op) `unexpectedDuring` "unOpSup"

assignOpSup PreDecr = FloatType
assignOpSup PreIncr = FloatType
assignOpSup PostDecr = FloatType
assignOpSup PostIncr = FloatType
assignOpSup op = ("operator " ++ show op) `unexpectedDuring` "assignOpSup"

binOpSup (ArithmeticOp Add) = FloatType
binOpSup (ArithmeticOp Sub) = FloatType
binOpSup (ArithmeticOp Mul) = FloatType
binOpSup (ArithmeticOp Mod) = IntType
binOpSup (ArithmeticOp Pow) = FloatType
binOpSup (ArithmeticOp Div) = FloatType
binOpSup (RelationalOp _) = FloatType
binOpSup (BooleanOp _) = BoolType

satisfiesUnOp op e =
  maybeBool
    (not (tpe `joinLeq` opSup))
    ( tpe,
      case op of
        Neg -> Right "numeric"
        Not -> Left opSup
        _ -> ("operator " ++ show op) `unexpectedDuring` "satisfiesUnOp"
    )
  where
    tpe = eType e
    opSup = unOpSup op

satisfiesAssignOp op e' = eType e `joinLeq` assignOpSup op && eSide e == LeftValue
  where
    e = assertEGeqStep 2 e'

satisfiesRef e'
  | PointerType _ <- eType e,
    LeftValue <- eSide e =
      True
  | otherwise = False
  where
    e = assertEGeqStep 2 e'

satisfiesDeref e'
  | PointerType _ <- eType e = True
  | otherwise = False
  where
    e = assertEGeqStep 2 e'

satisfiesBinOp op e1 e2 =
  ( maybeBool (not (tpe1 `joinLeq` opSup)) (tpe1, Left opSup),
    maybeBool (not (tpe2 `joinLeq` opSup)) (tpe2, Left opSup)
  )
  where
    tpe1 = eType (assertEGeqStep 2 e1)
    tpe2 = eType (assertEGeqStep 2 e2)
    opSup = binOpSup op
