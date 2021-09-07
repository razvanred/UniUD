-- Selezionare, in un file di testo, le stringhe di caratteri che rappresentano un numero in notazione romana. Solo i numeri romani devo essere stampati in uscita, separati da uno spazio, la restante parte del testo viene eliminata.

{
module Main (main) where
import System.Environment
import System.IO
import Control.Monad
}

%wrapper "basic"

tokens :-
  @units = (I[XV]|V?I{0,3})
  @mustUnits = (I[XV]|V|V?I{1,3})
  @tens = (X[CL]|L?X{0,3})
  @mustTens = (X[CL]|L|L?X{1,3})
  @hundreds = (C[MD]|D?C{0,3})
  @mustHundreds = (C[MD]|D|D?C{1,3})
  @thousands = M*
  @mustThousands = M+

  @caseA = @thousands @hundreds @tens @mustUnits
  @caseB = @thousands @hundreds @mustTens @units
  @caseC = @thousands @mustHundreds @tens @units
  @caseD = @mustThousands @hundreds @tens @units

  @caseA|@caseB|@caseC|@caseD {id}

  ~$white ; -- garbage
  $white+ ; -- whitespace chars

{

-- The token type:
-- data Token =
-- 	Number String

scan [] = return ()
scan (file:files) = do text <- file
                       putStrLn (unwords (alexScanTokens text))
                       scan files

main = do args <- getArgs
          files <- return (map readFile args)
          scan files
}
