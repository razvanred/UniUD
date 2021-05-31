-- In Scheme abbiamo due case: cond e case, il secondo è la tipica selezione multipla per uguaglianza, il primo è più generico
-- implemento il cond, così ha senso l'operatore di uguaglianza
-- In scheme sarebbe concesso scrivere case senza else, se sono esaustivi, ma questo sottolinguaggio non può essere esaustivo
-- quindi sarà forzato l'else, per evitare di aggiungere controllo di errore dinamico
-- In Scheme è possibile indicare sequenze di azioni nei cond e case (non è funzionale puro)
-- ma si tratta di zucchero sintattico per la funzione `begin` che non è richiesta dall'esercizio
-- quindi non verrà implementata
-- In Scheme abbiamo tre costrutti let: quello semplice, quello "incrementale" e quello mutualmente ricorsivo
-- dato che non è specificato, scelgo di implementare quello semplice per ovvie ragioni
-- In Scheme non è ammesso scrivere numeri attaccati, questo comportamento viene catturato da lexer
-- È stato necessario aggiungere il segno dei numeri nel lexer per ottenere un comportamento più simile a quello di Scheme

-- Alcune produzioni che sembrano strane od inutili sono così per eliminare conflitti shift/reduce ed un reduce/reduce
{
module Main
    where

import Happy7Lex
import System.Environment
import System.IO
import Control.Monad
}

%name scheme
%tokentype {Token}
%error {parseError}

%token
    int {Number $$}
    '+' {Plus}
    '*' {Times}
    '-' {Minus}
    eq {Equal}
    var {Id $$}
    '(' {Open}
    ')' {Close}
    '[' {SOpen}
    ']' {SClose}
    cond {Cond}
    let {Let}
    else {Else}

%nonassoc SIN
%nonassoc TWO
%right SEQ
%nonassoc SUB '+' '*'
%nonassoc NEG
%nonassoc '('
%%
Seq : Exp %prec SIN {[$1 []]}
    | Seq Exp %prec SEQ {$2 []: $1}

Exp : '(' '+' Sum ')' {$3}
    | '(' '-' Sub ')' %prec SUB {$3}
    | '(' '*' Mul ')' {$3}
    | var {\ env -> findVar $1 env}
    | int {const $1}
    | '(' '-' Exp ')' %prec NEG {\ env -> (- ($3 env))} -- Gli interi negativi non sono negazioni, e non puoi stackare negazioni senza parentesi
    | '(' '+' Exp ')' {$3} -- Su un solo elemento non fanno niente
    | '(' '*' Exp ')' {$3}
    | '(' let '(' List ')' Exp ')' {\ env -> ($6 ($4 env))}
    | '(' cond Branches '(' else Exp ')' ')' {terminateIf $3 $6}

Sum : Exp Exp %prec TWO {\ env -> ($1 env) + ($2 env)} -- In scheme le operazioni aritmetiche non sono binarie
    | Sum Exp %prec SEQ {\ env -> ($1 env) + ($2 env)} -- enby functions (:

Mul : Exp Exp %prec TWO {\ env -> ($1 env) * ($2 env)}
    | Mul Exp %prec SEQ {\ env -> ($1 env) * ($2 env)}

Sub : Exp Exp %prec TWO {\ env -> ($1 env) - ($2 env)}
    | Exp Sum %prec SEQ {\ env -> ($1 env) - ($2 env)} -- Nella sottrazione vengono sottratti al primo elemento tutti gli altri

List : '[' var Exp ']' {\ env -> ($2, ($3 env)):env}
     | List '[' var Exp ']' {\ env -> ($3, ($4 env)):($1 env)}

Branches : {- empty -} {const Nothing} -- L'else deve essere l'ultimo
         | Branches '(' '(' eq Exp Exp ')' Exp ')' {cascadeIf $1 (\ env -> ($5 env) == ($6 env)) $8}
       -- Ed anche oggi, si usa la right recursion domani
       -- Questa soluzione ritarda l'esecuzione di ogni cosa, anche le condizioni, di quanto più possibile

{
parseError = error "parse error"

cascadeIf :: ([(String, Integer)] -> Maybe Integer) -> ([(String, Integer)] -> Bool) -> ([(String, Integer)] -> Integer) -> [(String, Integer)] -> Maybe Integer
cascadeIf f cond action env = case (f env) of
                                Just val -> Just val
                                Nothing | cond env -> Just $ action env
                                        | otherwise -> Nothing

terminateIf :: ([(String, Integer)] -> Maybe Integer) -> ([(String, Integer)] -> Integer) -> [(String, Integer)] -> Integer
terminateIf f action env = case (f env) of
                             Just val -> val
                             Nothing -> action env

findVar :: String -> [(String, Integer)] -> Integer
findVar name [] = error "Undefined variable"
findVar name ((id, val):xs) | name == id = val
  | otherwise = findVar name xs

parseArgs :: [String] -> [IO String]
parseArgs [] = [getContents]
parseArgs list = map readFile list

execute :: [IO String] -> IO ()
execute [] = return ()
execute (x:xs) = do contents <- x
                    putStrLn $ unwords $ map show $ reverse $ scheme $ alexScanTokens contents
                    execute xs

main = do args <- getArgs
          contents <- return $ parseArgs args
          execute contents
}
