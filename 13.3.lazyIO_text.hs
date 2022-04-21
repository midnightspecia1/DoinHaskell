{-#LANGUAGE OverloadedStrings #-}
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as IOTL

toInts :: TL.Text  -> [Int]
toInts t = map read unpacked
        where unpacked = (map TL.unpack . TL.lines) t
              
main :: IO ()
main = do
    userInput <- IOTL.getContents 
    let numbers = toInts userInput
    IOTL.putStrLn ((TL.pack . show . sum) numbers)