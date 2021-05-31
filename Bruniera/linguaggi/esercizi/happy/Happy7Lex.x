{
module Happy7Lex(Token(Open, Close, SOpen, SClose, Id, Plus, Minus, Times, Equal, Cond, Let, Number, Else),alexScanTokens)
    where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [A-Za-z]
@ident = $alpha+
@number = (\+|\-)?$digit+
@err = (\+|\-)?$digit*(\-|\+) -- Cattura numeri attaccati

tokens :-

cond {const Cond}
let {const Let}
else {const Else}
\( {const Open}
\) {const Close}
\[ {const SOpen}
\] {const SClose}
@ident {\ s -> Id s}
\+ {const Plus}
\- {const Minus}
\* {const Times}
\= {const Equal}
@number {\ s -> Number $ plusRead s}
@err {const $ error "Syntax error"}
$white+ ;

{
-- Haskell non legge i `+`, ma Scheme si
plusRead ('+':xs) = read xs
plusRead xs = read xs

data Token = Open
           | Close
           | SOpen
           | SClose
           | Id String
           | Plus
           | Minus
           | Times
           | Equal
           | Cond
           | Let
           | Number Integer
           | Else
      deriving (Eq, Show)
}
