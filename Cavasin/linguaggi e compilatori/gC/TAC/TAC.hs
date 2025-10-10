{-# OPTIONS_GHC -Wno-unused-matches #-}

module TAC.TAC where

import AST
import Utils

type TAC = TacInstruction

data VarIdent = VarId {vLoc :: Position, vId :: String}
  deriving (Show, Eq)

instance PrettyPrinter VarIdent where
  pp (VarId vloc vid) =
    vid ++ "@" ++ case vloc of
      x -> show . fst $ x

data FunIdent = FunId {fLoc :: Position, fId :: String, numParameters :: Int}
  deriving (Show, Eq)

instance PrettyPrinter FunIdent where
  pp (FunId (line, _) fId numPar) = fId ++ " /" ++ show numPar

data XAddr
  = Addr {addr :: Addr}
  | ArrayAddr {base :: Addr, offset :: Addr}
  | RefAddr {addr :: Addr}
  deriving (Show, Eq)

instance PrettyPrinter XAddr where
  pp (Addr a) = pp a
  pp (RefAddr a) = pp a
  pp (ArrayAddr b o) = pp b ++ "[" ++ pp o ++ "]"

data Addr
  = ProgVar {progVar :: TacProgVariable, addrT :: Type}
  | TacLit {tacLit :: TacLit, addrT :: Type}
  | Temporary {tempInt :: Int, addrT :: Type}
  deriving (Show, Eq)

instance PrettyPrinter Addr where
  pp (ProgVar pv _) = pp pv
  pp (TacLit tL _) = pp tL
  pp (Temporary int _) = 't' : show int

data TacProgVariable
  = TacProgVar
  { varName :: VarIdent,
    varModality :: Modality,
    varType :: Type
  }
  deriving (Show, Eq)

instance PrettyPrinter TacProgVariable where
  pp (TacProgVar var _ _) = pp var

data TacLit
  = TacLitInt {contentInt :: Integer}
  | TacLitFloat Double
  | TacLitChar Char
  | TacLitBool {contentBool :: Bool}
  deriving (Show, Eq)

instance PrettyPrinter TacLit where
  pp (TacLitInt i) = show i
  pp (TacLitFloat f) = show f
  pp (TacLitChar c) = show c
  pp (TacLitBool b) = show b

data AddrPlusInt = AddrPlusInt
  deriving (Show, Eq)

instance PrettyPrinter AddrPlusInt where
  pp AddrPlusInt = show AddrPlusInt

data TacInstruction
  = TacBinary Addr BinaryOp Type Addr Addr -- binary operation assignments: “l = r1 bop r2”
  | TacPointerBinary Addr AddrPlusInt Type Addr Addr -- binary operation assignments for pointer arithmetic: “l = r1 bop r2”
  | TacUnary Addr UnaryOp Type Addr -- unary operation assignments: “l = uop r”
  | TacNullary Addr Type Addr -- nullary operation assignments: “l = r”
  | TacIndexedStore XAddr Type Addr -- Indexed store "a[i] =ty x"
  | TacIndexedLoad Addr Type XAddr -- Indexed load "x =ty a[i]"
  | TacPointerStore Addr Type Addr -- Indirect store "*x =ty y"
  | TacPointerLoad Addr Type Addr -- Indirect load "x =ty *y"
  | TacReferenceLoad Addr Type Addr -- "x = &y"
  | TacParam Type Addr -- Parameter of a call
  | TacReturn (Maybe (Type, Addr)) -- Return from a call
  | TacBoolCondJump Bool Addr Label -- Conditional jump on boolean guard
  | TacRelCondJump Addr RelOp Type Addr Label -- Conditional jump on comparison
  | TacUnCondJump Label -- Unconditional jump
  | TacCall (Maybe (Type, Addr)) FunIdent -- Call operation fcall f/n or pcall f/n
  | TacThrowException String
  deriving (Show, Eq)

instance PrettyPrinter TacInstruction where
  pp (TacBinary addr1 bop t addr2 addr3) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2 ++ " " ++ pp bop ++ " " ++ pp addr3
  pp (TacPointerBinary addr1 AddrPlusInt t addr2 addr3) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2 ++ " " ++ pp AddrPlusInt ++ " " ++ pp addr3
  pp (TacUnary addr1 Coercion _ addr2) = pp addr1 ++ " =" ++ "(" ++ pp (addrT addr1) ++ ")" ++ " " ++ pp addr2
  pp (TacUnary addr1 uop t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp uop ++ " " ++ pp addr2
  pp (TacNullary addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
  pp (TacIndexedStore addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
  pp (TacIndexedLoad addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
  pp (TacPointerStore addr1 t addr2) = "$" ++ pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
  pp (TacPointerLoad addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ "$" ++ pp addr2
  pp (TacReferenceLoad addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ "&" ++ pp addr2
  pp (TacParam t addr) = "param_" ++ pp t ++ " " ++ pp addr
  pp (TacReturn Nothing) = "return void"
  pp (TacReturn (Just (t, addr))) = "return " ++ pp addr
  pp (TacBoolCondJump True addr label) = "if True" ++ " " ++ pp addr ++ " jump to " ++ pp label
  pp (TacBoolCondJump False addr label) = "if False" ++ " " ++ pp addr ++ " jump to " ++ pp label
  pp (TacRelCondJump addr1 relop t addr2 label) = "if " ++ pp addr1 ++ " " ++ pp relop ++ " " ++ pp addr2 ++ " jump to " ++ pp label
  pp (TacUnCondJump label) = "goto " ++ pp label
  pp (TacCall Nothing f) = "pcall " ++ pp f
  pp (TacCall (Just (t, a)) f) = pp a ++ " =" ++ pp t ++ " " ++ pp f
  pp (TacThrowException string) = "throw exception " ++ string

data Label
  = LabInstr Int
  | LabTrueOr Int
  | LabFalseAnd Int
  | LabIfFalse Int
  | LabEmpty
  | LabFunId String Int Int
  | LabGuard Int
  | LabBodyStart Int
  | FALL
  deriving (Show, Eq)

instance PrettyPrinter Label where
  pp (LabInstr i) = "Instr" ++ show i 
  pp (LabTrueOr i) = "TrueOr" ++ show i
  pp (LabFalseAnd i) = "FalseAnd" ++ show i
  pp (LabIfFalse i) = "IfFalse" ++ show i
  pp (LabEmpty) = ""
  pp (LabFunId s i1 i2) = "Fun_" ++ s ++ "@" ++ "(" ++ show i1 ++ "," ++ show i2 ++ ")" 
  pp (LabGuard i) = "Guard" ++ show i 
  pp (LabBodyStart i) = "Body" ++ show i
  pp (FALL) = ""

