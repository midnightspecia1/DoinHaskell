--25.1 difference between characters and bytes (unicode)
{-#LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as E
import System.Environment (getArgs)

getDiff :: B.ByteString  -> T.Text 
getDiff bString = "Characters count: " <>
                T.pack (show chars)    <>
                ", "                   <>
                "BytesCount: "         <>
                T.pack (show bytes)    <>
                ", "                   <>
                "Difference: "         <>
                T.pack (show difference)
    where   bytes = B.length bString
            chars = (T.length . E.decodeUtf8) bString
            difference = bytes - chars

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    bstring <- B.readFile fileName
    TI.putStrLn (getDiff bstring)
    