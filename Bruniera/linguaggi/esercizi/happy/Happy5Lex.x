{
module Happy5Lex(Token(Number,Plus,Times,Minus,Equal,If,Then,Else,Open,Close),alexScanTokens)
    where
}
%wrapper "basic"

$digit = [0-9]
@num = $digit+

identifier :-

@num {\ s -> Number $ read s}
\+ {const Plus}
\* {const Times}
\- {const Minus}
\=\= {const Equal}
\( {const Open}
\) {const Close}
if {const If}
then {const Then}
else {const Else}
$white+ ;

{
data Token = Number Integer
           | Plus
           | Times
           | Minus
           | Equal
           | If
           | Then
           | Else
           | Open
           | Close
           deriving (Eq, Show)
}
