{
module Main
    where

import Happy5Lex
import System.Environment
import System.IO
import Control.Monad
}

%name hask
%tokentype {Token}
%error {parseError}

%token
    int {Number $$}
    '+' {Plus}
    '*' {Times}
    '-' {Minus}
    eq {Equal}
    if {If}
    then {Then}
    else {Else}
    '(' {Open}
    ')' {Close}

%nonassoc SIN
%right SEQ
%right ITE -- Priorità più bassa, tende ad essere l'elemento top-level
%left eq
%left '+' '-'
%left '*'
%nonassoc NEG -- Non associare, se ci sono problemi preferisco che dia errore
%%
Seq : Exp %prec SIN {[$1]}
    | Seq Exp %prec SEQ {$2 : $1}

Exp : Exp1 {$1}
    | ITE {$1}
    | '(' Exp ')' {$2}

ITE : if Cond then Exp else Exp %prec ITE {if $2 then $4 else $6}

Cond : Exp eq Exp {$1 == $3}

Exp1 : Exp '+' Exp {$1 + $3}
     | Exp '-' Exp {$1 - $3}
     | Exp '*' Exp {$1 * $3}
     | int {$1}
     | '-' Exp %prec NEG {(-$2)}

{
parseError = error "parse error"

parseArgs :: [String] -> [IO String]
parseArgs [] = [getContents]
parseArgs list = map readFile list

execute :: [IO String] -> IO ()
execute [] = return ()
execute (x:xs) = do contents <- x
                    putStrLn $ unwords $ map show $ reverse $ hask $ alexScanTokens contents
                    execute xs

main = do args <- getArgs
          contents <- return $ parseArgs args
          execute contents
}
