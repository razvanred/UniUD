-- Sequenze di espressioni in linguaggio Scheme formate da:
-- numeri interi
-- operazioni aritmetiche: +, *, -
-- test di uguaglianza: =
-- costrutti if e case
-- L’analizzatore deve valutare le espressioni ricevute in ingresso.

{
module Lex9(Token(..), scan) where
}
%wrapper "basic"

$digit = [0-9]
@num = $digit+

tokens :-
    @num {\ s -> TokenNumber (read s)}
    \+ {const TokenPlus}
    [\-] {const TokenMinus}
    [\*] {const TokenTimes}
    [\/] {const TokenDivided}
    \= {const TokenEqual}
    if {const TokenIf}
    case {const TokenCase}
    else {const TokenElse}
    \[ {const TokenCaseOpen}
    \] {const TokenCaseClose}
    \( {const TokenOpen}
    \) {const TokenClose}
    $white+ ;

{
data Token = TokenNumber Integer
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDivided
           | TokenEqual
           | TokenIf
           | TokenCase
           | TokenElse
           | TokenCaseOpen
           | TokenCaseClose
           | TokenOpen
           | TokenClose
           deriving (Eq, Show)

scan = alexScanTokens
}