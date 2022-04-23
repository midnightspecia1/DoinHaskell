import System.IO

main :: IO ()
main = do
    myFile <- openFile "salut.txt" ReadMode
    endFile <- hIsEOF myFile
    firstLine <- if not endFile
                 then hGetLine myFile
                 else return "empty"
    endFile <- hIsEOF myFile
    secondLine <- if not endFile
                  then hGetLine myFile
                  else return "empty"

    putStrLn "done!"

--hGetLine - works like getLine but need to pass handle as an argument
--hPutStrLn - same as putStrLn but recieving handle as an arg 

--hGetLine - handle is asumed to be stdin
--hPutStrLn - handle is asumed to be stdout

--handy functions
--hIsEOF - checks is this file comes to an end
--readFile - read file, outputs IO String
--writeFile - write file, outputs IO ()
--appendFile - appends file with String arg, outputs IO ()

