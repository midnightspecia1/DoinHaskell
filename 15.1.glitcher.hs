import System.Environment
import System.Random
import Control.Monad
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


randomChar :: IO Char 
randomChar = do
    randomInt <- randomRIO (0, 255)
    return (toEnum randomInt)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString 
sortSection start size bytes = mconcat [before, new, after]
                where (before, rest) = BC.splitAt start bytes
                      (midle, after) = BC.splitAt size rest
                      new = BC.reverse midle

randomSortSection :: BC.ByteString -> IO BC.ByteString 
randomSortSection bytes = do
    let length = BC.length bytes
    let size = 25
    start <- randomRIO (1, length - size)
    return(sortSection start size bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString] 
glitchActions = [randomReplaceByte,
                 randomSortSection,
                 randomReplaceByte,
                 randomSortSection]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "done!"