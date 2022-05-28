import qualified Data.Map as Map

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
readGrade = do
        line <- getLine
        let grade = read line
        return grade
--readGrade = getLine >>= (return . read)

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

assessCandidateIO :: IO String
assessCandidateIO = do
        candidate <- readCandidate
        let passed = viable candidate
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement

-- List context - processing a lsit of candidates
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                        , codeReview = A
                        , cultureFit = A
                        , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                        , codeReview = C
                        , cultureFit = A
                        , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                        , codeReview = A
                        , cultureFit = B
                        , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1, candidate1)
                           , (2, candidate2)
                           , (3, candidate3) ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id = do
        candidate <- Map.lookup id candidateDB
        let passed = viable candidate
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement

-- 31.4
failedOrPassed :: Maybe String -> String
failedOrPassed Nothing = "error, id not found"
failedOrPassed (Just val) = val 

-- List context - processing a list of candidates
candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
        candidate <- candidates
        let passed = viable candidate
        let statement = if passed
                        then "passed"
                        else "failed"
        return statement

-- monadic assessCnadidates that works on Maybe, IO and Lists
assessCandidate :: Monad m => m Candidate -> m String 
assessCandidate candidates = do
        candidate <- candidates
        let passed = viable candidate1
        let statement = if passed 
                        then "passed"
                        else "failed"
        return statement
