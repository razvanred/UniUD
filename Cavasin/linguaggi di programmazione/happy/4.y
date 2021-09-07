-- Il linguaggio delle espressioni aritmetiche nella forma usata nelle scuole medie, ossia
-- numeri interi,
-- 4 operazioni aritmetiche,
-- le tre forme di parantesi: tonde, quadre, graffe.
-- L’analizzatore deve controllare che le parentesi siano inserite secondo le usuali regole: parentesi tonde più interne, seguite dalle quadre, e poi graffe. L’analizzatore deve inoltre valutare l’espressione in ingresso.

{
module Main (main) where
import Lex4
import System.Environment
import System.IO
import Control.Monad
}

%name calc
%tokentype {Token}
%error {parseError}

%token
    int {Number $$}
    '+' {Plus}
    '-' {Minus}
    '*' {Times}
    '/' {Divided}
    '=' {Equal}
    if {If}
    case {Case}
    else {Else}
    '[' {CaseOpen}
    ']' {CaseClose}
    '(' {Open}
    ')' {Close}

%left '+' '-'
%left '*' '/'

%%

CurlyExp : CurlyExp '+' CurlyExp {$1 + $3}
         | CurlyExp '-' CurlyExp {$1 - $3}
         | CurlyExp '*' CurlyExp {$1 * $3}
         | CurlyExp '/' CurlyExp {$1 `quot` $3}
         | Curly {$1}

SquareExp : SquareExp '+' SquareExp {$1 + $3}
          | SquareExp '-' SquareExp {$1 - $3}
          | SquareExp '*' SquareExp {$1 * $3}
          | SquareExp '/' SquareExp {$1 `quot` $3}
          | Square {$1}

RoundExp : RoundExp '+' RoundExp {$1 + $3}
         | RoundExp '-' RoundExp {$1 - $3}
         | RoundExp '*' RoundExp {$1 * $3}
         | RoundExp '/' RoundExp {$1 `quot` $3}
         | Round {$1}

Exp : Exp '+' Exp {$1 + $3}
    | Exp '-' Exp {$1 - $3}
    | Exp '*' Exp {$1 * $3}
    | Exp '/' Exp {$1 `quot` $3}
    | int {$1}

Curly : '{' SquareExp '}' {$2}
      | Square {$1}

Square : '[' RoundExp ']' {$2}
       | Round {$1}

Round : '(' Exp ')' {$2}
      | int {$1}

{
parseError = error "parse error"

run [] = return ()
run (file:files) = do text <- file
                      putStrLn $ text ++ " = " ++ show (calc (scan text))
                      run files

main = do args <- getArgs
          files <- return (map readFile args)
          run files
}
