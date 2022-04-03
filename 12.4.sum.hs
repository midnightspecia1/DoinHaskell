import System.Environment
import Control.Monad

--mapM - special version of map that operates on any Monad type
--mapM_ - another version, its works like mapM but trows the results
--replicateM - function that repeat some action n number of times
--typicaly in haskell when function ends with and underscore it trows out the results

main:: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)

exMain :: IO ()
exMain = do
    vals <- mapM (\_ -> getLine) [1..3]
    mapM_ putStrLn vals
    