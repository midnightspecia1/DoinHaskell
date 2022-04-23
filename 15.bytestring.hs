{-#LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

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