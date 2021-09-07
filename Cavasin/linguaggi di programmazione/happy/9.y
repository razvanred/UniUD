-- Sequenze di espressioni in linguaggio Scheme formate da:
-- numeri interi
-- operazioni aritmetiche: +, *, -
-- test di uguaglianza: =
-- costrutti if e case
-- L’analizzatore deve valutare le espressioni ricevute in ingresso.

{
module Main (main) where
import Lex9
import System.Environment
import System.IO
import Control.Monad
}

%name calc
%tokentype {Token}
%error {parseError}

%token
    int {TokenNumber $$}
    '+' {TokenPlus}
    '-' {TokenMinus}
    '*' {TokenTimes}
    '/' {TokenDivided}
    '=' {TokenEqual}
    if {TokenIf}
    case {TokenCase}
    else {TokenElse}
    '[' {TokenCaseOpen}
    ']' {TokenCaseClose}
    '(' {TokenOpen}
    ')' {TokenClose}

%left '+' '-'
%left '*' '/'

%%

Block : '(' '+' Plus ')' {$3}
      | '(' '-' Minus ')' {$3}
      | '(' '*' Times ')' {$3}
      | '(' '/' Divided ')' {$3}
      | '(' if Cond Block Block ')' {if $3 then $4 else $5}
      | '(' case Block Cases Default ')' {calcCase $3 (reverse ($5:$4))}
      | '(' case Block Default ')' {snd $4}
      | int {$1}

Cond : '(' '=' Equal ')' {allEqual $3}

Default : '[' else Block ']' {([], $3)}

Cases : Cases Case {$2:$1}
      | Case {[$1]}

Case : '[' '(' CaseValues ')' Block ']' {($3, $5)}

CaseValues : CaseValues int {$2:$1}
           | int {[$1]}

Plus : Plus Block {$1+$2}
     | Block {$1}

Minus : Block Plus {$1-$2}
      | Block {(-$1)}

Times : Times Block {$1*$2}
      | Block {$1}

Divided : Divided Block {$1 `quot` $2}
        | Block {$1}

Equal : Equal Block {$2:$1}
      | Block {[$1]}

{
allEqual (e1:e2:l) = e1 == e2 && allEqual (e2:l)
allEqual _ = True

calcCase _ [] = error "Internal error"
calcCase val [(_, clause)] = clause
calcCase val ((values, clause) : clauses) = if val `elem` values then clause else calcCase val clauses

parseError = error "parse error"

run [] = return ()
run (file:files) = do text <- file
                      putStrLn text
                      print (calc (scan text))
                      run files

main = do args <- getArgs
          files <- return (map readFile args)
          run files
}
