import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int
type WildCoLtd = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                        ,("KINGinYELLOW",15000)
                        ,("dagon1997",300)
                        ,("rcarter1919",12)
                        ,("xCTHULHUx",50000)
                        ,("yogSOThoth",150000)] 

gamerIdDB :: Map.Map WildCoLtd GamerId
gamerIdDB = Map.fromList [(1001,1)
                         ,(1002,2)
                         ,(1003,3)
                         ,(1004,4)
                         ,(1005,5)
                         ,(1006,6)]               

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

creditsFromWCId :: WildCoLtd -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits 

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits un = Map.lookup un creditsDB

lookupGamerId :: WildCoLtd -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

-- signature of the function that lookup credits with gamerid is 
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
-- or more general variant 
-- f a -> (a -> f b) -> f b

-- we have to write wrapper for this - its not a good sign
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = Map.lookup username creditsDB

-- so with this function we can put it all together 
-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)

-- with Maybe type you can always make a wraper but as example IO not so friendly
-- echo IO action
-- echo :: IO ()
-- IO String -> (String -> IO()) -> IO()
-- f a -> (a -> f b) -> f b

-- with an operator >>= (bind) we can chain together actions in IO, or any do notation actions
echo :: IO ()
echo = getLine >>= putStrLn

-- now with <$> <*> pure and >>= we can chain together any computations we want
