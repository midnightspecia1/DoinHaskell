{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Encoding(utf8, setLocaleEncoding)

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text 
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text 
highlight query text = T.intercalate highlighted pieces
            where highlighted = mconcat ["{" , query, "}"]
                  pieces = T.splitOn query text

main :: IO ()
main = do
    setLocaleEncoding utf8
    TIO.putStrLn (highlight dharma bgText)
    