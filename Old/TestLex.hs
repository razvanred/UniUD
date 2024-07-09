import LexEP
import System.Environment ( getArgs )
import Data.Foldable (for_)

main :: IO ()
main = do
  args <- getArgs
  for_ (map show lexer(head(args))) putStrLn
--   tokens <- lexer(head(args))
--   mapM putStrLn tokens



 
  