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
import Data.Char
}

%wrapper "basic"

$exp = [A-Za-Z]

tokens :-

~$white+ {id} -- Ignora tutte le sequenze che non rispettano l'espressione
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

espandi [] = []
espandi (x:xs) = '(':(toUpper x):'|':(toLower x):')':(espandi xs)

-- Analizza le stringe e stampa i numeri romani che trova
-- Uso `unwords` per comporre la lista in una stringa separata da spazi
analyzeContents :: [IO String] -> IO ()
analyzeContents [] = return ()
analyzeContents (x:xs) = do contents <- x
                            putStrLn $ unlines $ map espandi $ alexScanTokens contents
                            analyzeContents xs

-- Leggo gli argomenti
-- Leggo i contenuti corrispondenti agli argomenti
-- Analizzo i contenuti letti
main = do args <- getArgs
          contents <- return $ parseArgs args
          analyzeContents contents
}
