module Main where

import ConstantSolver.Solver
import ErrorCollector.Main
import Parser.ASTBuilder
import Parser.Par (myLexer, pBlock1)
import TAC.TACInstruction
import TAC.TACutils (genCode, printOutput, printProgram)
import TypeChecker.Main
import Utils (prp)

-- import Control.Monad (when)
-- import System.Environment (getArgs)
-- import System.Exit (exitFailure)
-- import Prelude (
--     Either (..),
--     FilePath,
--     IO,
--     Int,
--     Show,
--     String,
--     concat,
--     getContents,
--     mapM_,
--     putStrLn,
--     readFile,
--     show,
--     unlines,
--     ($),
--     (++),
--     (.),
--     (>),
--     (>>),
--     (>>=), Bool (..),
--  )

-- import Parser.Abs ()
-- import Parser.Lex (Token, mkPosToken)
-- import Parser.Par (myLexer, pBlock1)
-- import Parser.Print (Print, printTree)
-- import ConstantSolver.Solver
-- import ErrorCollector.Main
-- import Parser.ASTBuilder
-- import Parser.Par (myLexer, pBlock1)
-- import TAC.TACInstruction
-- import TAC.TACutils (genCode, printOutput, printProgram)
-- import TypeChecker.Main
-- import Utils (prp)
-- type Err = Either String
-- type ParseFun a = [Token] -> Err a
-- type Verbosity = Int

-- putStrV :: Verbosity -> String -> IO ()
-- putStrV v s = when (v > 1) $ putStrLn s

-- runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
-- runFile v p f = putStrLn f >> readFile f >>= run v p

-- run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
-- run v p s =
--     case p ts of
--         Left err -> do
--             putStrLn "\nParse              Failed...\n"
--             putStrV v "Tokens:"
--             mapM_ (putStrV v . showPosToken . mkPosToken) ts
--             putStrLn err
--             exitFailure
--         Right tree -> do
--             putStrLn "\nParse Successful!"
--             showTree v tree
--   where
--     ts = myLexer s
--     showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]

-- showTree :: (Show a, Print a) => Int -> a -> IO ()
-- showTree v tree = do
--     putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
--     putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

-- usage :: IO ()
-- usage = do
--     putStrLn $
--         unlines
--             [ "usage: Call with one of the following argument combinations:",
--               "  --help          Display this help message.",
--               "  (no arguments)  Parse stdin verbosely.",
--               "  (files)         Parse content of files verbosely.",
--               "  -s (files)      Silent mode. Parse content of files silently."
--             ]

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         ["--help"] -> usage
--         [] -> getContents >>= run 2 pBlock1
--         "-s" : fs -> mapM_ (runFile 0 pBlock1) fs
--         fs -> mapM_ (runFile 2 pBlock1) fs

main :: IO ()
main = do
    contents <- readFile "tests/demo.txt"
    case pBlock1 $ myLexer contents of
        Left x -> putStrLn x
        Right pt -> do
            let t = buildTree pt
            let t1 = solveConstants 100 t
            let t2 = staticAnalizer t1
            case collectErrors True t2 of
                Left msgs -> do
                    -- prp t2
                    prp msgs
                Right t3 -> do
                    let t4 = tacProgram t3
                    putStrLn "---demo"
                    prp t3
                    printOutput t4
