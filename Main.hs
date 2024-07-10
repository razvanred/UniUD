import Data.Foldable (for_)
import System.Environment
import System.IO
import TestString

main = do
    args <- getArgs
    mapM filecontent args

filecontent t = do
    handle <- openFile t ReadMode
    contents <- hGetContents handle
    func (lines contents)
    hClose handle

-- let filelines = lines contents
--  in map putStrLn filelines

func :: [String] -> IO ()
func inputs = for_ (map (show . test) inputs) putStrLn
