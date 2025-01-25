module TACEmitter.TAC where
import AST

type TAC = TacInstruction

data VarIdent = VarId{ vLoc :: Position, vId :: String}
    deriving(Show,Eq)
 
data FunIdent = FunId { fLoc :: Position, fId :: String, numParameters :: Int }
  deriving (Show,Eq)
 
data XAddr = Addr Addr
           | ArrayAddr { base :: Addr , offset :: Addr }
           | RefAddr Addr
 
data Addr = ProgVar { progVar :: TacProgVariable, addrT :: Type }
          | TacLit { tacLit :: TacLit , addrT :: Type }
          | Temporary { tempInt :: Int , addrT :: Type }
 deriving (Show,Eq)

data TacProgVariable
  = TacProgVar {
    varName      :: VarIdent,
    --varOwningFun :: FunIdent,
    --varIsMutable :: Bool,
    varModality  :: Modality,
    varType :: Type}
  deriving (Show,Eq)
 
data TacLit
   = TacLitInt Integer
   | TacLitFloat Double
   | TacLitChar Char
   | TacLitBool Bool
  deriving(Show,Eq)
 
 
data TacInstruction
  = TacBinary Addr BinaryOp Type Addr Addr
  | TacUnary Addr UnaryOp Type Addr
  | TacNullary Addr Type Addr
  | TacJumpUnc
  | TacJumpBool
  | TacJumpRel
  | TacIndexedStore TacProgVariable Addr Type Addr -- ^ Indexed store (a[i] =ty x)
  | TacIndexedLoad Addr Type TacProgVariable Addr  -- ^ Indexed load (x =ty a[i])
  | TacPointerStore Addr Type Addr  -- ^ Indirect store (*x =ty y)
  | TacPointerLoad Addr Type Addr  -- ^ Indirect load (x =ty *y)
  | TacReferenceLoad Addr Type Addr
  | TacParam Type Addr  -- ^ Parameter of a call
  | TacReturn (Maybe (Type, Addr))  -- ^ Return from a call
  | TacCall (Maybe (Type, Addr)) FunIdent -- ^ Call operation t = fcall f/n or pcall f/n
   deriving(Show,Eq)

 
 
-- binary operation assignments: “l = r1 bop r2 ”
-- unary operation assignments: “l = uop r” (including coercions)
-- nullary operation assignments: “l = r”
-- unconditional jump: “goto label ”
-- boolean-valued conditional jump: “if r goto label ” and “ifFalse r goto label ”
-- relational conditional jump: “if r1 rel r2 goto label ”
-- indexed copy assignments: “l = id [r]” and “id [r1 ] = r2 ” (absolute indexes, not relative to types)
-- reference and dereferenced assignments: “l = &id ”, “l1 = ∗l2 ” and “∗l = r”
-- functions/procedures