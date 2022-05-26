
data Grade = F | D | C | B | A deriving (Ord, Eq, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Ord, Eq, Enum, Show, Read)

data Candidate = Candidate 
                { candidateId :: Int
                , codeReview :: Grade
                , cultureFit :: Grade
                , education :: Degree } deriving Show

-- weather candidat viable or not
viable :: Candidate -> Bool
viable c = all (== True) tests
        where passedCoding = codeReview c > B
              passedCultureFit = cultureFit c > C
              educationMin = education c >= MS
              tests = [passedCoding
                      ,passedCultureFit
                      ,educationMin]

testCandidate :: Candidate
testCandidate = Candidate {candidateId = 1
                         , codeReview = A
                         , cultureFit = A
                         , education = PhD }

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
        putStrLn "enter id: "
        cId <- readInt
        putStrLn "enter code grade: "
        codeGrade <- readGrade
        putStrLn "enter culture fit grade: "
        culture <- readGrade
        putStrLn "enter education: "
        education <- readDegree
        return (Candidate { candidateId = cId
                          , codeReview = codeGrade
                          , cultureFit = culture
                          , education = education})

