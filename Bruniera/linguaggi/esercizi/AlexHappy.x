-- I file erano, ovviamente, separati. All'inizio di ogni file è commentato il nome che aveva
-- Per integrare lexer e parser negli esercizi su Happy si usano i moduli, quindi serve
-- utilizzare nomi adeguati, perché ghc li riconosca.

-- Questa è la funzione fornita che genera le coppie di esercizi da svolgere
generaraCoppia nEsercizi matricola = (primo, secondo) where 
  primo = matricola `mod` nEsercizi + 1
  secondo = (matricola `mod` (nEsercizi - 3) + primo + 1) `mod`  nEsercizi + 1
-- Ed i risultati dell'esecuzione (la matricola è 142491)
-- Lexer: (4, 9)
-- Parser: (7, 5)











--------------------------------------------------------------------------------------
    -- Alex4.x
--------------------------------------------------------------------------------------

-- I file di input sono passati come argomenti
-- Se non ci sono argomenti, legge da stdin
{
-- I numerali non possono essere spezzati su più righe
-- I numerali devono essere maiuscoli (i, ii, iii, non sono validi)
-- I numerali devono essere "isolati", ovvero possono essere circondati solo da spazi bianchi
module Main where

import System.Environment
import System.IO
import Control.Monad
}

%wrapper "basic"

-- Questa espressione è presa da geeksforgeeks, è piuttosto lungo 
-- M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})
-- Per assicurarsi che che non sia vuota bisogna copiarla 4 volte (una per ogni simbolo intero)
-- ed in ciascuna volta forzare uno dei simboli
-- Più, aggiungere un caso in cui è forzato il simbolo mezzo (V,L,D)
-- È stato anche rimosso il limite massimo delle migliaia
@ones = M*(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{1,3}|V)
@tens = M*(CM|CD|D?C{0,3})(XC|XL|L?X{1,3}|L)(IX|IV|V?I{0,3})
@hundreds = M*(CM|CD|D?C{1,3}|D)(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})
@thousands = M+(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})
-- Un'espressione è l'unione delle 4
@exp = (@ones|@tens|@hundreds|@thousands)

tokens :-

@exp {id} -- Non ho token diversi da gestire, posso direttamente restituire la stringa
~$white+ ; -- Ignora tutte le sequenze che non rispettano l'espressione
-- Se avessi usato il punto al posto del `~$white` per catturare tutti gli altri caratteri
-- Una volta trovato uno spazio bianco, il lexer avrebbe catturato anche il resto della riga
-- Anche se il resto avesse contenuto un numero romano
$white+ ; -- Ignora gli spazi bianchi

{
-- Se ci sono argomenti, legge i file passati come argomento
-- Altrimenti l'egge dallo standard input
parseArgs :: [String] -> [IO String]
parseArgs [] = [getContents]
parseArgs list = map readFile list

-- Analizza le stringe e stampa i numeri romani che trova
-- Uso `unwords` per comporre la lista in una stringa separata da spazi
analyzeContents :: [IO String] -> IO ()
analyzeContents [] = return ()
analyzeContents (x:xs) = do contents <- x
                            putStrLn $ unwords $ alexScanTokens contents
                            analyzeContents xs

-- Leggo gli argomenti
-- Leggo i contenuti corrispondenti agli argomenti
-- Analizzo i contenuti letti
main = do args <- getArgs
          contents <- return $ parseArgs args
          analyzeContents contents
}








--------------------------------------------------------------------------------------
    -- Alex9.x
--------------------------------------------------------------------------------------


-- I file di input sono passati come argomenti
-- Se non ci sono argomenti, legge da stdin
{
-- Tutti i sostantivi saranno normalizzati in minuscolo
-- Ogni sostantivo verrà stampato su una nuova riga
-- Le eccezioni che cambiano genere in base al numero, vengono trattate
-- In base al genere in cui vengono incontrate
module Main where

import System.Environment
import System.IO
import Control.Monad
import Data.Char
}

%wrapper "basic"

$aw = [A-Za-z\ \t\n\f\v\r]
$alpha = [A-Za-z]
@word = $alpha+ --sequenza di lettere dell'alfabeto
@nword = ~$white* ~$aw+ ~$white* --una sequenza di caratteri non bianchi, tra cui un carattere non alpha
-- Uso un programma Alex che trasforma parole nella loro versione case insensitive
@exc1 = (B|b)(R|r)(A|a)(C|c)(C|c)(I|i)(A|a)
@exc2 = (M|m)(A|a)(N|n)(O|o)
@exc3 = (D|d)(I|i)(T|t)(A|a)
@exc4 = (G|g)(I|i)(N|n)(O|o)(C|c)(C|c)(H|h)(I|i)(A|a)
@exc5 = (V|v)(I|i)(T|t)(I|i)
@exc6 = (L|l)(I|i)(M|m)(O|o)(N|n)(I|i)
@exc7 = (P|p)(A|a)(L|l)(M|m)(E|e)
@exc8 = (P|p)(A|a)(L|l)(M|m)(A|a)
@exc9 = (U|u)(O|o)(V|v)(O|o)
@exc10 = (V|v)(I|i)(T|t)(E|e)
tokens :-

-- Non ho token diversi da gestire, posso direttamente restituire la stringa
@exc1 {const "braccio"} -- Eccezioni
@exc2 {const "mani"}
@exc3 {const "dito"}
@exc4 {const "ginocchio"}
@exc5 {const "viti"}
@exc6 {const "limone"}
@exc7 {const "palma"}
@exc8 {const "palme"}
@exc9 {const "uova"}
@exc10 {const "viti"}
@word {normalize} -- Caso regolare
@nword ;--{normalize} -- Ignora tutte le sequenze che non sono parole (contengono simboli)
$white+ ; -- Ignora gli spazi bianchi

{
-- Normalizza e trasforma i sostantivi regolari
-- Aggiunge l'acca passando da femminile singolare a plurale
-- Rimuove l'acca passando da maschile plurale a singolare
normalize :: [Char] -> [Char]
normalize [x,y]
  | (toLower x) == 'c' && (toLower y) == 'a' = "che"
  | (toLower x) == 'g' && (toLower y) == 'a' = "ghe"
  | otherwise = (toLower x):(normalize [y])
normalize [x,y,z]
  | (toLower x) == 'c' && (toLower y) == 'h' && (toLower z) == 'i' = "co"
  | (toLower x) == 'g' && (toLower y) == 'h' && (toLower z) == 'i' = "go"
  | otherwise = (toLower x):(normalize [y,z])
normalize [x] | (toLower x) == 'a' = ['e']
              | (toLower x) == 'i' = ['o']
              | otherwise = [toLower x]
normalize (x:xs) = (toLower x):(normalize xs)

-- Se ci sono argomenti, legge i file passati come argomento
-- Altrimenti l'egge dallo standard input
parseArgs :: [String] -> [IO String]
parseArgs [] = [getContents]
parseArgs list = map readFile list

-- Analizza le stringhe e stampa i numeri romani che trova
-- Uso `unwords` per comporre la lista in una stringa separata da spazi
analyzeContents :: [IO String] -> IO ()
analyzeContents [] = return ()
analyzeContents (x:xs) = do contents <- x
                            putStrLn $ unlines $ alexScanTokens contents
                            analyzeContents xs

-- Leggo gli argomenti
-- Leggo i contenuti corrispondenti agli argomenti
-- Analizzo i contenuti letti
main = do args <- getArgs
          contents <- return $ parseArgs args
          analyzeContents contents
}







--------------------------------------------------------------------------------------
    -- Happy5Lex.x
--------------------------------------------------------------------------------------

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






--------------------------------------------------------------------------------------
    -- Happy5Parse.y
--------------------------------------------------------------------------------------

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






--------------------------------------------------------------------------------------
    -- Happy7Lex.x
--------------------------------------------------------------------------------------

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






--------------------------------------------------------------------------------------
    -- Happy7Parse.y
--------------------------------------------------------------------------------------

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
