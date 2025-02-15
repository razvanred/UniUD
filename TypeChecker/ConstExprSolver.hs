module TypeChecker.ConstExprSolver (solveConstExpr) where

import AST
import Algebra.Lattice ((\/))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isLatin1)
import Data.Int (Int32)
import TypeChecker.TypeUtils
import Utils hiding (Endo)
import Prelude hiding (error, id)

-- unpackers

unpackBool (BoolLiteral _ v Step2{sType = BoolType}) = v
unpackBool lt = "literal " ++ show lt `unexpectedDuring` "unpackToBool"

unpackChar (CharLiteral _ v Step2{sType = CharType}) = v
unpackChar lt = "literal " ++ show lt `unexpectedDuring` "unpackToChar"

unpackInt (IntLiteral _ v Step2{sType = IntType}) = v
unpackInt lt = "literal " ++ show lt `unexpectedDuring` "unpackToInt"

unpackFloat (FloatLiteral _ v Step2{sType = FloatType}) = v
unpackFloat lt = "literal " ++ show lt `unexpectedDuring` "unpackToFloat"

biunpackBool lt1 lt2 = (unpackBool lt1, unpackBool lt2)

biunpackPromoteChar lt1 lt2 = (promote lt1, promote lt2)
  where
    promote (BoolLiteral _ v Step2{sType = BoolType}) = toEnum $ fromEnum v
    promote (CharLiteral _ v Step2{sType = CharType}) = v
    promote lt = "literal " ++ show lt `unexpectedDuring` "biunpackPromoteChar"

biunpackPromoteInt lt1 lt2 = (promote lt1, promote lt2)
  where
    promote (BoolLiteral _ v Step2{sType = BoolType}) = fromIntegral $ fromEnum v
    promote (CharLiteral _ v Step2{sType = CharType}) = fromIntegral $ fromEnum v
    promote (IntLiteral _ v Step2{sType = IntType}) = v
    promote lt = "literal " ++ show lt `unexpectedDuring` "biunpackPromoteToInt"

biunpackPromoteFloat lt1 lt2 = (promote lt1, promote lt2)
  where
    promote (BoolLiteral _ v Step2{sType = BoolType}) = fromIntegral $ fromEnum v
    promote (CharLiteral _ v Step2{sType = CharType}) = fromIntegral $ fromEnum v
    promote (IntLiteral _ v Step2{sType = IntType}) = fromIntegral v
    promote (FloatLiteral _ v Step2{sType = FloatType}) = v
    promote lt = "literal " ++ show lt `unexpectedDuring` "biunpackPromoteToFloat"

isZero (FloatLiteral _ v Step2{sType = FloatType}) = v == 0
isZero (CharLiteral _ v Step2{sType = CharType}) = v == 0
isZero (IntLiteral _ v Step2{sType = IntType}) = v == 0
isZero (BoolLiteral _ v Step2{sType = IntType}) = not v
isZero lt = "literal " ++ show lt `unexpectedDuring` "isZero"

-- operators

fFloatOp (ArithmeticOp Add) = Just (+)
fFloatOp (ArithmeticOp Sub) = Just (-)
fFloatOp (ArithmeticOp Mul) = Just (*)
fFloatOp (ArithmeticOp Pow) = Just pow
fFloatOp (ArithmeticOp Div) = Just divide
fFloatOp _ = Nothing

fIntOp (ArithmeticOp Mod) = Just mod
fIntOp _ = Nothing

fBBoolOp (BooleanOp Or) = Just (||)
fBBoolOp (BooleanOp And) = Just (&&)
fBBoolOp _ = Nothing

fRelationalOp (RelationalOp NotEq) = Just (/=)
fRelationalOp (RelationalOp GreaterThanEq) = Just (>=)
fRelationalOp (RelationalOp GreaterThan) = Just (>)
fRelationalOp (RelationalOp LessThanEq) = Just (<=)
fRelationalOp (RelationalOp LessThan) = Just (<)
fRelationalOp (RelationalOp Eq) = Just (==)
fRelationalOp _ = Nothing

-- helpers

checkLiteral e
  | isLiteral e = case ann e of
      Step2{sType = ErrorType} -> False
      _ -> True
  | otherwise = False

rStep2 = fillOutStep2 RightValue

eRStep2 tpe = pass1 updateAnn (rStep2 tpe . ann)

solveConstExpr = astEmap idty idty idty solveExpr

fixRange expr@(CharLiteral pos c x)
  | isLatin1 c = expr
  | otherwise = LiteralOutOfRange (void expr) |< CharLiteral pos '?' x
fixRange expr@(IntLiteral pos num x)
  | num == fromIntegral @Int32 (fromIntegral num) = expr
  | otherwise = LiteralOutOfRange (void expr) |< IntLiteral pos (num `mod` 2 ^ (32 :: Integer)) x
fixRange expr@(FloatLiteral{}) = expr -- since isIEEE=True
fixRange expr = expr

solveExpr expr@(IntLiteral{}) = fixRange $ eRStep2 IntType expr
solveExpr expr@(CharLiteral{}) = fixRange $ eRStep2 CharType expr
solveExpr expr@(StringLiteral{}) = fixRange $ eRStep2 StringType expr
solveExpr expr@(FloatLiteral{}) = fixRange $ eRStep2 FloatType expr
solveExpr expr@(BoolLiteral{}) = fixRange $ eRStep2 BoolType expr
solveExpr expr@(UnaryOp pos op lt x)
  | checkLiteral lt =
      case satisfiesUnOp op lt of
        Nothing -> solveUnOp (eType lt)
        Just (got, expected) -> TypeMismatch got expected |< eRStep2 ErrorType expr
  | otherwise = expr -- left on Step1
  where
    solveUnOp tpe
      | CharType <- tpe = makeCharLit negate
      | IntType <- tpe = makeIntLit negate
      | FloatType <- tpe = makeFloatLit negate
      | BoolType <- tpe = makeBoolLit not
      | otherwise = "mismatch" `unexpectedDuring` "solveUnOp"
      where
        newX = rStep2 tpe x |<> ann lt
        makeCharLit op = CharLiteral pos (op (unpackChar lt)) newX
        makeIntLit op = IntLiteral pos (op (unpackInt lt)) newX
        makeFloatLit op = FloatLiteral pos (op (unpackFloat lt)) newX
        makeBoolLit op = BoolLiteral pos (op (unpackBool lt)) newX
solveExpr expr@(BinaryOp pos op lt1 lt2 x)
  | checkLiteral lt1 && checkLiteral lt2 =
      case satisfiesBinOp op lt1 lt2 of
        (Nothing, Nothing) ->
          if (ArithmeticOp Div == op || ArithmeticOp Mod == op) && isZero lt2
            then
              DivisionBy0 |< eRStep2 sup expr
            else
              solveBinOp
        (err1, err2) ->
          let f = maybe idty $ \(got, expected) -> (TypeMismatch got expected |<)
          in  f err1 . f err2 $ eRStep2 ErrorType expr
  | otherwise = expr -- left on Step1
  where
    sup = eType lt1 \/ eType lt2
    solveBinOp
      | FloatType <- sup,
        Just op <- fFloatOp op =
          makeFloatLit op
      | CharType <- sup,
        Just op <- liftA2 (<|>) fFloatOp fIntOp op =
          makeCharLit op
      | IntType <- sup,
        Just op <- liftA2 (<|>) fFloatOp fIntOp op =
          makeIntLit op
      | FloatType <- sup,
        Just op <- fRelationalOp op =
          makeBoolLit (biunpackPromoteFloat lt1 lt2) (op :: Double -> Double -> Bool)
      | IntType <- sup,
        Just op <- fRelationalOp op =
          makeBoolLit (biunpackPromoteInt lt1 lt2) (op :: Integer -> Integer -> Bool)
      | CharType <- sup,
        Just op <- fRelationalOp op =
          makeBoolLit (biunpackPromoteChar lt1 lt2) (op :: Char -> Char -> Bool)
      | Just op <- fBBoolOp op =
          makeBoolLit (biunpackBool lt1 lt2) op
      | otherwise = "mismatch" `unexpectedDuring` "solveBinOp"
      where
        newX = rStep2 sup x |<> ann lt2 |<> ann lt1
        makeCharLit op = CharLiteral pos (uncurry op (biunpackPromoteChar lt1 lt2)) newX
        makeIntLit op = IntLiteral pos (uncurry op (biunpackPromoteInt lt1 lt2) `mod` 2 ^ (32 :: Integer)) newX
        makeFloatLit op = FloatLiteral pos (uncurry op (biunpackPromoteFloat lt1 lt2)) newX
        makeBoolLit vs op = BoolLiteral pos (uncurry op vs) newX
solveExpr expr = expr
