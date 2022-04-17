{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup()

--Text is not lazy in haskell, if we need so can use 
--Data.Text.Lazy

--pack - function that converts String to Text
--unpack - converts Text to String

firstWord :: String 
firstWord = "optimist"

secondWord :: T.Text 
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- This not compile because Text can't be initialized with a String
-- someText :: T.Text 
-- someText = "Sometext"

-- OverloadedStrings - extension that can fix the issue above
-- can be used in compilation with -X
-- ghc text.hs -XOverloadedStrings
-- it can be used as an argument in ghci
-- it can be used with pragma {-# LANGUAGE OverloadedStrings #-}

-- other usefull extensions
-- ViewPatterns - Allows more sophisticated pattern matching
-- TemplateHaskell - Provides tools for Haskell metaprogramming
-- DuplicateRecordFields - Solves problem when using same name for record causing conflict
-- NoImplicitPrelude - Allows use custom Prelude

someText :: T.Text 
someText = "Some text"

main :: IO ()
main = do
    print someText

-- Text functions
sampleinput :: T.Text 
sampleinput = "this\nis\ninput"

-- T.lines - divide text to list of Text
-- T.words - same as lines but works on any whitespace characters rather than only \n
-- T.splitOn Text Text - split up text by any substring of text
-- T.unwords ([Text]) - reverse for words
-- T.unlines ([Text]) - reverse for lines
-- T.intercalate Text ([Text]) - reverse for splitOn (put fst arg in between every list member adn concat)

breakText :: T.Text 
breakText = "simple"

exampleText :: T.Text 
exampleText = "This is simple to do"

-- ++ defined only for List type and wont work for Text
-- Text has monoid and semigroup instance so we can use mconcat and <>

combinedTextMonoid :: T.Text 
combinedTextMonoid = mconcat ["some", " " , "text"]

combinedTextSemigroup :: T.Text 
combinedTextSemigroup = "some" <> " " <> "text"

-- 23.3 ownLines and ownUnlines
ownLines :: T.Text -> [T.Text]
ownLines t = T.splitOn "\n" t 

ownUnlines :: [T.Text] -> T.Text 
ownUnlines l = T.intercalate "\n" l