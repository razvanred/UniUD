{
module Main where  
import LexEP
import AbsDef
}

%name pE E
%name pS S
%tokentype { Token }
%error { parseError }

%token
  int { TokenInt $$ }
  double {TokenDouble $$ }
  string { TokenString $$ } 
  '+' { TokenPlus }
  '*' { TokenTimes }
  '(' { OpenPar }
  ')' { ClosedPar }
  
%%

E :: { Tree Int Double }
E : E1 { $1 } | E2 { $1 }

E1 :: { Tree Int Double }
E1 : E2 '+' ListE2 { Chain ($1 : $3) }

ListE2 :: { [Tree Int Double] }
ListE2 : E2 { (:[]) $1 } | E2 '+' ListE2 { (:) $1 $3 }

E2 :: { Tree Int Double }
E2 : E3 { $1 } | I '*' E4 { Repeat $1 $3 }

E4 :: { Tree Int Double }
E4 : '(' E1 ')' { $2 } | E3 { $1 }

E3 :: { Tree Int Double }
E3 : Double { Leaf $1 }

Double  :: { Double }
Double   : double  { $1 }

-------------

I :: { Int }
I : Integer { $1 }
Integer :: { Int }
Integer  : int  { $1 }

-------------

S :: { Tree Int String }
S : S1 { $1 } | S2 { $1 }

S1 :: { Tree Int String }
S1 : S2 '+' ListS2 { Chain ($1 : $3) }

ListS2 :: { [Tree Int String] }
ListS2 : S2 { (:[]) $1 } | S2 '+' ListS2 { (:) $1 $3 }

S2 :: { Tree Int String }
S2 : S3 { $1 } | I '*' S4 { Repeat $1 $3 }

S4 :: { Tree Int String }
S4 : '(' S1 ')' { $2 } | S3 { $1 }

S3 :: { Tree Int String }
S3 : String { Leaf $1 }

String  :: { String }
String   : string  { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

main = getContents >>= print . calc . lexer
}
