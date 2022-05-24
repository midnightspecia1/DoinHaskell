import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

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

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = 

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits un = Map.lookup un creditsDB

-- signature of the function that lookup credits with gamerid is 
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
-- or more general variant 
-- f a -> (a -> f b) -> f b

-- we have to write wrapper for this - its not a good sign
altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = Map.lookup username creditsDB

-- so with this function we can put it all together 
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = altLookupCredits (lookupUserName id)

-- with Maybe type you can always make a wraper but as example IO not so friendly
-- echo IO action
-- echo :: IO ()
-- IO String -> (String -> IO()) -> IO()
-- f a -> (a -> f b) -> f b
