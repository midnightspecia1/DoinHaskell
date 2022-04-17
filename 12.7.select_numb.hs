--22.2 quotes
quotes :: [String]
quotes = ["If you no longer go for a gap that exists, you are no longer a racing driver.",
          "I am not designed to come second or third. I am designed to win.",
          "Being second is to be the first of the ones who lose.",
          "The past is just data. I only see the future.",
          "I have no idols. I admire work, dedication and competence."]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n" : xs) = []
lookupQuote (x : xs) = quote : (lookupQuote xs)
        where quote = quotes !! ((read x) - 1)

main :: IO ()
main = do
    input <- getContents
    mapM_ putStrLn (lookupQuote (lines input))