{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Data.Foldable (for_)
import System.Environment
import System.IO
import ParString
import ExpectedTestsResults
import Utils


-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         ["--help"] -> usage
--         [] -> getContents >>= run 2 pE
--         "-s" : fs -> mapM_ (runFile 0 pE) fs
--         fs -> mapM_ (runFile 2 pE) fs

main = do
    args <- getArgs
    case args of
        "-test" : testFile:_ -> runTests testFile
        -- _ -> mapM filecontent args

-- filecontent t = do
--     handle <- openFile t ReadMode
--     contents <- hGetContents handle
--     func (lines contents)
--     hClose handle

-- func :: [String] -> IO ()
-- func inputs = for_ (map (show . pE . myLexer) inputs) putStrLn

runTests f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle
    let matches = checkEquality  (map  (pE . myLexer)  (lines contents))  stringResults
    for_ (map (\match->case match of
        (Left error)-> error
        (Right t)->if t then "Ok" else "Invalid abstract tree") matches) putStrLn
    hClose handle 




