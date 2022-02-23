import Text.XHtml (height)



x :: Int 
x = 2 

y :: Integer 
y = 3

letter :: Char 
letter = 'a'

string :: String 
string = "asd"

someDouble :: Double 
someDouble = 0.1235

someB :: Bool 
someB = True 

--List types
characters :: [Char]
characters = ['a','s','e']

values :: [Double]
values = [0.12, 0.125, 0.546]

--string in haskell is the syninymous to the type of [Char]
--those are axact same things
dogName :: String 
dogName = "Bobby"

anotherDogName :: [Char]
anotherDogName = "Niggy"

--tupples 
heightAndLength :: (Int, Int)
heightAndLength = (12, 45)

firstAndLastName :: (String, String, Char)
firstAndLastName = ("Drew", "Ganger", 'a')

streetNAdress :: (Int, String)
streetNAdress = (35, "Custom. Str")

--Function types

someFunc :: Int -> Int 
someFunc a = a*2

--converting from one type to another
half :: Int -> Double 
half a = fromIntegral a / 2

--String to and from fucntions show and read
printDouble :: Int -> String 
printDouble a = show (a*2)

anotherNumber :: Int
anotherNumber = read "6" 

--when we have something like this 
--z = read "6"
--haskell dont know what to do, what type to return ?
--we can proceed with explicit type signature like above
--or we can add q = z/2 (context) 
--and now haskell knows what to return

--we can add return type at the end of the function call
a = read "6" :: Int
b = read "6" :: Double

--Functions with multiple arguments
--reason that the signature for multiple arguments functions looks like this
--is that the under the hood of the compiler function like this its a 
--series of the nested lamda functions
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

makeAddressLambda = \number ->
                        \street ->
                            \town ->  (number, street, town)

--types for first class functions 
ifEven :: (Int -> Int) -> Int -> Int 
ifEven f n = if even n
             then f n
             else n

--type variables
--any lower case names in type signatures
--show that the functions  getting any type 
--on that place
simple :: a -> a
simple n = n
--type variables intead of representing the value
--they representing a type
makeMore :: a -> b -> c -> (a, b, c)
makeMore z x c = (z, x, c)

--type signature for map is
--map :: (a -> b) -> [a] -> [b]
--it cannot be 
--map :: (a -> a) -> [a] -> [a]
--because main feature of the map it is not iteration but
--transforming of a list of one type to list of another type

--type signature for filter
--filter :: (a -> bool) -> [a] -> [a]

--type signature for tail
--tail :: [a] -> [a]

--type signature for head
myHead :: [a] -> a
myHead (x:xs) = x
myHead [] = error "list empty"

myTail :: [a] -> [a]
myTail (x:xs) = xs
myTail [] = []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x


--type synonyms
patientInfo :: PatientName -> Age -> Height -> String 
patientInfo patient age height = name ++ ageHeight
                      where   name = fst patient ++ ". " ++ snd patient
                              ageHeight = "Age " ++ show age ++ ". Height" ++ show height ++ ". "
--we can create type synonyms with word "type"
type Age = Int 
type Height = Int 
type FirstName = String 
type LastName = String 

--it is possible to use tupples for representing some types
type PatientName = (String, String)
--accesing PatientName values
getFirstName :: PatientName -> FirstName
getFirstName patient = fst patient

getLastName :: PatientName -> LastName
getLastName patient = snd patient

--Creating your types with keyword data
data Sex = Male | Female
--the word Sex is a type costructor 
--here its only a type name but later we will see that that can take arguments
--Male and Female its an instances of data constructors
--data Bool = True | False defined similarly

--using pattern matching to create function that returns sex char
sexInitial :: Sex -> String 
sexInitial Male = "M"
sexInitial Female = "F"

--lets define blood type
data RhType = Positive | Negative
data ABOType = A | B | AB | O 
data BloodType = BloodType ABOType RhType

--now we can create BloodType of the patients
patient1BT :: BloodType
patient1BT = BloodType A Positive

patient2BT :: BloodType
patient2BT = BloodType O Negative

patient3BT :: BloodType
patient3BT = BloodType B Positive

--making print function for Rh ABOType and Bloodtype
showRh :: RhType -> String 
showRh Positive = "Positive"
showRh Negative = "Negative"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String 
showBloodType (BloodType abo rh) = "BloodType is " ++ showABO abo ++ " " ++ showRh rh

--defining canDonate function
canDonate :: BloodType -> BloodType -> Bool 
canDonate (BloodType O _) _ = True 
canDonate _ (BloodType AB _) = True 
canDonate (BloodType B _) (BloodType B _) = True 
canDonate (BloodType A _) (BloodType A _) = True 
canDonate _ _ = False 

--canDonate with patients args
canDonatePatients :: Patient -> Patient -> Bool 
canDonatePatients patientOne patientTwo = canDonate (bloodType patientOne) (bloodType patientTwo)


--defining Name with or without middlename
type MiddleName = String 
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String 
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Andrew" "Digital"
name2 = NameWithMiddle "Ganger" "Ivanovich" "Bottler"

--now we create patiend data type
--data Patient = Patient Name Sex Int Int Int BloodType

john :: Patient
john = Patient (NameWithMiddle "John" "Gangsterovich" "Travolting") Male 70 146 80 (BloodType O Positive)

--record syntax
data Patient = Patient { name :: Name ,
                          sex :: Sex ,
                          age :: Int ,
                       weight :: Int ,
                       heigth :: Int ,
                    bloodType :: BloodType }

eva :: Patient
eva = Patient {  sex = Female,
                name = NameWithMiddle "Eva" "Jumbo" "gumbo",
                 age = 20,
              weight = 50,
              heigth = 176,
           bloodType = BloodType O Positive }

bumba = Patient {  sex = Female,
                name = NameWithMiddle "Bumba" "Pikardin" "Kella",
                 age = 20,
              weight = 50,
              heigth = 176,
           bloodType = BloodType AB Positive }

patientSummary :: Patient -> String 
patientSummary patient = spacer ++ "\n" ++ nameP ++ "\n" ++ sexP ++ "\n" ++ ageP ++ "\n" ++
                         weightP ++ "\n" ++ heightP ++ "\n" ++ bloodTypeP ++ "\n" ++ spacer 
                         where
                            spacer = "+++++++++++++++++++++++++++++" 
                            nameP = "Patients name: " ++ showName (name patient)
                            sexP = "Sex: " ++ sexInitial (sex patient)
                            ageP = "Age: " ++ show (age patient)
                            weightP = "Weight: " ++ show (weight patient)
                            heightP = "Height: " ++ show (heigth patient)
                            bloodTypeP = "BloodType: " ++  showBloodType (bloodType patient)
