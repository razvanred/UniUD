import Data.Foldable (for_)
import System.Environment
import System.IO
import TestString
import AbsDef

main = do
    args <- getArgs
    mapM filecontent args

filecontent t = do
    handle <- openFile t ReadMode
    contents <- hGetContents handle
    func (lines contents)
    hClose handle

func :: [String] -> IO ()
func inputs = for_ (map (show . test) inputs) putStrLn

