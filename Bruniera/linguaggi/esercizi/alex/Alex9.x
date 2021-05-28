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
