
-- using <*> to create data in a context
data User = User 
            { name :: String 
            , gamerId :: Int 
            , score :: Int 
            } deriving Show

-- creating a user in a context of Maybe
-- we will pretend that data gathered from some database (where it can be missed)
serverUsername :: Maybe String
serverUsername = Just "Sue"

serverUserId :: Maybe Int
serverUserId = Just 1337

serverScore :: Maybe Int 
serverScore = Just 9001

-- to create Maybe User from this data we can use <$> (functor) and <*> (applicative)
newUser :: Maybe User
newUser = User <$> serverUsername <*> serverUserId <*> serverScore

-- using applicative to create types inside IO
readInt :: IO Int 
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user
