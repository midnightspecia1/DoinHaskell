{-#LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment (getArgs)

getDiff :: T.Text -> B.ByteString -> T.Text 
getDiff text bString = mconcat ["Characters count: ",
                                  ls,
                                  ", ",
                                  "BytesCount: ",
                                  lbs,
                                  ", ",
                                  "Difference: ",
                                  difference]
                    where ls = T.length text
                          lbs = BC.length bString
                          difference = ls - lbs

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    string <- readFile fileName
    let text = E.encodeUtf8 (T.pack string)
    biteString <- B.readFile fileName
    
    putStrLn (getDiff string biteString)
    