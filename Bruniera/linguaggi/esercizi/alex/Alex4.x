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
