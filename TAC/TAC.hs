module TAC.TAC where
import AST
import PrettyPrinter

type TAC = TacInstruction

data VarIdent = VarId{ vLoc :: Position, vId :: String}
    deriving(Show,Eq)

instance PrettyPrinter VarIdent where 
pp (VarId vloc vid) = vid ++ "@" ++ case vloc of
    Nothing -> ""
    Just x ->show . fst $ x


data FunIdent = FunId { fLoc :: Position, fId :: String, numParameters :: Int }
  deriving (Show,Eq)
 
data XAddr = Addr Addr
           | ArrayAddr { base :: Addr , offset :: Addr }
           | RefAddr Addr
 
data Addr = ProgVar { progVar :: TacProgVariable, addrT :: Type }
          | TacLit { tacLit :: TacLit , addrT :: Type }
          | Temporary { tempInt :: Int , addrT :: Type }
 deriving (Show,Eq)

instance PrettyPrinter Addr where
pp (ProgVar pv _) = printProgVar pv
pp (TacLit tL _) = printTacLit tL
pp (Temporary int _) = 't' : show int


data TacProgVariable
  = TacProgVar {
    varName      :: VarIdent,
    --varOwningFun :: FunIdent,
    --varIsMutable :: Bool,
    varModality  :: Modality,
    varType :: Type}
  deriving (Show,Eq)


instance PrettyPrinter TacProgVariable where 
pp (TacProgVar var _ _) = pp var


data TacLit
   = TacLitInt Integer
   | TacLitFloat Double
   | TacLitChar Char
   | TacLitBool Bool
  deriving(Show,Eq)
 
instance PrettyPrinter TacLit where
pp(TacLitInt i) = show i  
pp(TacLitFloat f) = show f
pp(TacLitChar c) = show c
pp(TacLitBool b) = show b

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
  | TacReferenceLoad Addr Type Addr -- ^  x = &y 
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

instance PrettyPrinter TacInstruction
pp (TacBinary addr1 bop t addr2 addr3) =  pp addr1 ++ " " ++ pp bop ++ " " ++ pp t ++ " " ++ pp addr2 ++ pp addr3
pp (TacUnary addr1 uop t addr2) = pp addr1 ++ " " ++ pp uop ++ " " ++ pp addr2
pp (TacNullary addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " pp addr2 
pp (TacIndexedStore tacProg addr1 t addr1) = pp tacProg ++ " " ++ pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
pp (TacPointerStore addr1 t addr2) = "$" ++ pp addr1 ++ " =" ++ pp t ++ " " ++ pp addr2
pp (TacPointerLoad addr1 t addr2) = pp addr1 ++ " =" ++ pp t ++ " " ++ "$" ++ pp addr2
pp (TacReferenceLoad addr1 t addr2) = pp addr1 ++ " " ++ " =" ++ " " ++ "&" ++ pp addr2
pp (_) = ""
