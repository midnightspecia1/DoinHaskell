--24.1 Unix cp programm (copy file int some directory and optionaly rename it)
{-#LANGUAGE OverloadedStrings #-}
import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

getFileName :: T.Text -> T.Text
getFileName fn = last (T.splitOn "/" fn)

getPath :: T.Text -> T.Text
getPath path = T.dropEnd fileLength path 
            where fileName = getFileName path
                  fileLength = T.length fileName

isOnlyName :: T.Text -> Bool
isOnlyName t = (T.length . getPath) t == 0

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TI.readFile fileName
    let newFileName = (T.pack . head . tail) args

--  with this program create file in a directory of the source 
--  if newfile dir not specified, so this is optional
    let secondFile = if isOnlyName newFileName
                     then T.concat [getPath (T.pack fileName), newFileName]
                     else newFileName
    TI.appendFile (T.unpack secondFile) input
    TI.putStrLn secondFile
