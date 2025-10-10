{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import ExpectedTestsResults
import GHC.Float (expDouble)
import ParDouble
import System.Environment
import Utils

{-
permette di lanciare il parser per Tree Int Double specificando dei file da command line
con l'opzione -test il file in input viene confrontato con i risultati attesi contenuti in ExpectedTestsResults.hs
senza opzione viene lanciato il parser sulle linee del file in input
-}
main = do
    args <- getArgs
    case args of
        "-test" : testFile : _ -> do
            inputs <- fileContents testFile
            mapM_ putStrLn $ f inputs
          where
            f inputs = concat $ zipWith (\a b -> [b, a]) (runTests parse doubleResults inputs) inputs
        _ -> do
            inputs <- concat <$> mapM fileContents args
            mapM_ (putStrLn . extract . parse) inputs

parse = pE . myLexer
