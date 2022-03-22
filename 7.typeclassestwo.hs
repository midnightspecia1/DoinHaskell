--making types with "and" and "or"
--product types - combining types with "and"
--sum types - combining types with "or"

--common sum type is 
data Bool = False | True 

--Using sum types to define Name with or without middle name
type FirstName = String 
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName 
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName 

--Creator Author and Artist depends of a Name type but to extend it
--we need only to add new definiton of the Name with or  
data Creator = AuthorCreator Author | ArtistCreator Artist 
data Author = Author Name
data Artist = Person Name | Band String

--we can't make the same price for Book and VinylRecord because
--haskell authomaticaly make for them getter like this 
--price :: Book -> Double
--price (Book _ _ _ _ val) = val
--
--price :: VinylRecord -> Double
--price (VinylRecord _ _ _ _ val) = val
--and this is two conflicting functions

data Book = Book {
        author :: Creator ,
        isbn :: String , 
        bookTitle :: String , 
        bookYear :: Int ,
        bookPrice :: Double 
    }

data VinylRecord = VinylRecord {
        artist :: Creator ,
        recordTitle :: String , 
        recordYear :: Int , 
        recordPrice :: Double   
}

data CollectibleToy = CollectibleToy {
    name :: String ,
    toyDescription :: String ,
    toyPrice :: Double
}

data Pamphlet = Pamphlet {
    tittle :: String ,
    pamphletDescription :: String ,
    organization :: String 
}

data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | FreePamphlet Pamphlet

--now we can make price function that can safely take any of those types
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem vinyl) = recordPrice vinyl
price (ToyItem toy) = toyPrice toy
price (FreePamphlet _) = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = "Made by: " ++ show (author book)
madeBy (RecordItem vinyl) = "Made by: " ++ show (artist vinyl)
madeBy (ToyItem toy) = "Made by: " ++ show (name toy)
madeBy (FreePamphlet pamphlet) = "Made by: " ++ show (organization pamphlet)

--making show to madeBy function work
instance Show Name where
    show (Name first last) = first ++ " " ++ last 
    show (NameWithMiddle first middle last) = first ++ " " ++ middle ++ " " ++ last
    show (TwoInitialsWithLast one two last) = [one] ++ ". " ++ [two] ++ ". " ++ last

instance Show Artist where
    show (Person name) = show name
    show (Band bandName) = show bandName

instance Show Author where
    show (Author name) = show name

instance Show Creator where
    show (AuthorCreator author) = show author
    show (ArtistCreator artist) = show artist

johnyBoy :: Creator
johnyBoy =  ArtistCreator 
                    (Person
                        (NameWithMiddle "Johny" "Dabldor" "Boy"))

--creating Shape type
type Width = Float 
type Height = Float
type Radius = Float 

data Shape = SquareShape Width 
           | RectangleShape Width Height
           | CircleShape Radius deriving Show

perimiter :: Shape -> Float
perimiter (SquareShape w) = w * 4
perimiter (RectangleShape w h) = w * 2 + h * 2
perimiter (CircleShape r) = 2 * pi * r

area :: Shape -> Float
area (SquareShape w) =  w ^ 2
area (RectangleShape w h) = w * h
area (CircleShape r) = pi * r ^ 2