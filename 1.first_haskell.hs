

messyMain :: IO ()
messyMain=do
    putStrLn "Who is recipient ?"
    recipient <- getLine
    putStrLn "Book name: "
    title <- getLine
    putStrLn "Who is message author? "
    author <- getLine
    putStrLn ("Dear" ++ recipient ++ "\n"
            ++ "Thanks for buying \" "
            ++ title ++ "\"!\n Sincerly, \n" ++ author)