{-#LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

sampleBytes :: B.ByteString 
sampleBytes = "Hello!"

sampleString :: String 
sampleString = BC.unpack sampleBytes

-- B.unpack :: ByteString -> [GHC.Word.Word8] - that converts bytestring to list of bytes 
-- BC.unpack :: ByteString -> [Char] - this converts bytestring to string
bcInt :: BC.ByteString
bcInt = "6"

toInt :: BC.ByteString -> Int
toInt = read . BC.unpack


nagarjunaBC :: BC.ByteString 
nagarjunaBC = "नागर्जुनॅ"

nagarjunaText :: T.Text
nagarjunaText = "првиет"

nagarjunaB :: B.ByteString 
nagarjunaB = (BC.pack . T.unpack) nagarjunaText

-- Data.Text.Encoding module provide two general functions
-- E.encodeUtf8 :: T.Text -> BC.ByteString 
-- E.decodeUtf8 :: BC.ByteString -> T.Text
-- with them we can safely convert unicode text to bytestrings and vise versa

nagarjunaSafe :: B.ByteString 
nagarjunaSafe = E.encodeUtf8 nagarjunaText 

