{
module Main where  
import LexDouble
import AbsDef
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  int { TokenInt $$ }
  double {TokenDouble $$ }
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

I :: { Int }
I : Integer { $1 }

Double  :: { Double }
Double   : double  { $1 }

Integer :: { Int }
Integer  : int  { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

main = getContents >>= print . calc . lexer
}
