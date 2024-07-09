{
module LexEP (Token(..), lexer) where
}

%wrapper "basic"

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \+ | \* | \( | \)

tokens :-

  $white+                       ;
  $d+				{ \s -> TokenInt (read s) }
  $d+ \. $d+ (e (\-)? $d+)?    	{ \s -> TokenDouble (read s) }
  \" ([$u # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t | r | f)))* \" { \s -> TokenString s } 
  \(				{ \s -> OpenPar }
  \)				{ \s -> ClosedPar }
  \+				{ \s -> TokenPlus }
  \*				{ \s -> TokenTimes }

{
data Token
  = OpenPar
  | ClosedPar
  | TokenPlus
  | TokenTimes
  | TokenInt Int
  | TokenDouble Double
  | TokenString String
  deriving (Eq, Show)

lexer = alexScanTokens
}
