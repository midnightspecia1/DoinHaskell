{-#LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI


getCounts :: T.Text  -> (Int, Int, Int)
getCounts s = (charCount, wordCount, lineCount)
        where charCount = T.length s
              wordCount = (length . T.words) s
              lineCount = (length . T.lines) s

printCounts :: (Int, Int, Int) -> T.Text  
printCounts (cc, wc, lc) = T.unwords ["chars: ", T.pack (show cc) 
                                  , " words: ", T.pack (show wc)
                                  , " lines: ", T.pack (show lc) ]

--strict version of the program (because Text is non-lazy type)
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TI.readFile fileName 
    let count = (printCounts . getCounts) input
    TI.appendFile "stats.dat" (mconcat [T.pack fileName, " ", count, "\n"])
    TI.putStrLn count
    
    
