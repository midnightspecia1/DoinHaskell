--24.2 proggram that capitalize text in file
{-#LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI


main :: IO ()
main = do
    args <- getArgs 
    let fileName = head args
    fileHandle <- openFile fileName ReadMode 
    input <- hGetContents fileHandle
    let capitalized = T.toUpper (T.pack input)
    (print . T.length) capitalized
    hClose fileHandle
    writeFile fileName (T.unpack capitalized)
    
