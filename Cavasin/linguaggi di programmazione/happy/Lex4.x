-- Il linguaggio delle espressioni aritmetiche nella forma usata nelle scuole medie, ossia
-- numeri interi,
-- 4 operazioni aritmetiche,
-- le tre forme di parantesi: tonde, quadre, graffe.
-- L’analizzatore deve controllare che le parentesi siano inserite secondo le usuali regole: parentesi tonde più interne, seguite dalle quadre, e poi graffe. L’analizzatore deve inoltre valutare l’espressione in ingresso.

{
module Lex4(Token(..), scan) where
}
%wrapper "basic"

$digit = [0-9]
@num = $digit+

tokens :-
    @num {\ s -> Number (read s)}
    \+ {const Plus}
    [\-−] {const Minus}
    [\*⋅] {const Times}
    [\/:] {const Divided}
    \( {const Open1}
    \) {const Close1}
    \[ {const Open2}
    \] {const Close2}
    \{ {const Open3}
    \} {const Close3}
    $white+ ;

{
data Token = Number Integer
           | Plus
           | Minus
           | Times
           | Divided
           | Open1
           | Close1
           | Open2
           | Close2
           | Open3
           | Close3
           deriving (Eq, Show)

scan = alexScanTokens
}