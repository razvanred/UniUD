-- Ricevuto in input una sequenza di sostantivi nelle lingua italiana, trasformare al plurale i sostantivi femminili e al singolare quelli maschili. Il programma può applicare una semplice regola generale per catalogare un sostantivo come femminile-maschile, singolare-plurale ma deve riuscire a gestire almeno una decina di eccezioni a questo regola (es. mano, braccia).

{
module Main (main) where
import System.Environment
import System.IO
import Control.Monad
import Data.Char
}

%wrapper "basic"

tokens :-
  ampio {const "ampli"}
  ala {const "ali"}
  alpaca {const "alpaca"}
  amico {const "amici"}
  arma {const "armi"}
  asparago {const "asparagi"}
  belga {const "belgi"}
  blu {const "blu"}
  braccio {const "braccia"}
  bue {const "buoi"}

  [a-z]+ {pluralize}
  ~$white+ ; -- garbage
  $white+ ; -- whitespace chars

{

-- The token type:
-- data Token =
-- 	Number String

pluralize [x,y]
  | x == 'c' && y == 'a' = "che"
  | x == 'g' && y == 'a' = "ghe"
  | otherwise = x:(pluralize [y])
pluralize [x,y,z]
  | x == 'c' && y == 'h' && z == 'i' = "co"
  | x == 'g' && y == 'h' && z == 'i' = "go"
  | otherwise = x:(pluralize [y,z])
pluralize [x] | x == 'a' = "e"
              | x == 'i' = "o"
              | otherwise = [x]
pluralize (x:xs) = x:(pluralize xs)

scan [] = return ()
scan (file:files) = do text <- file
                       putStrLn (unwords (alexScanTokens (map toLower text)))
                       scan files

main = do args <- getArgs
          files <- return (map readFile args)
          scan files
}

