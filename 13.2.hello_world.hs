{-# LANGUAGE OverloadedStrings #-}
 import qualified Data.Text as T
 import qualified Data.Text.IO as TIO

 helloPerson :: T.Text -> T.Text 
 helloPerson name = "Hello" <> " " <> name <> "!"

 main :: IO ()
 main = do
     TIO.putStrLn "Enter your name"
     name <- getLine
     let greetings = helloPerson (T.pack name)
     TIO.putStrLn greetings
