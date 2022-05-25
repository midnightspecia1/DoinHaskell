
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
testCandidate = Candidate ( 1 , D , B , PhD )