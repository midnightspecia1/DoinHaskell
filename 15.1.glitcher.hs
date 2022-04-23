import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


--functions that can generate random ByteStrings
-- safeInt ensures that the generated char would be in limit of one byte
intToChar :: Int -> Char 
intToChar int = toEnum safeInt
        where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString 
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString 
replaceByte loc charVal bytes = mconcat [before, new, after]
                where (before, rest) = BC.splitAt loc bytes
                      after = BC.drop 1 rest
                      new = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString 
randomReplaceByte bytes = do
    let bytesLen = BC.length bytes
    location <- randomRIO (1, bytesLen)
    charVal <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- randomReplaceByte imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "done!"