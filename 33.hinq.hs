
import Control.Monad
import Control.Applicative

data Name = Name {
     firstName :: String
    ,lastName :: String }

instance Show Name where
    show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
                | Sophmore
                | Junior
                | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student {
         studentId :: Int
        ,gradeLevel :: GradeLevel
        ,studentName :: Name } deriving Show

students :: [Student]
students = [Student 1 Senior (Name "Audre" "Lorde")
        ,Student 2 Junior (Name "Leslie" "Silko")
        ,Student 3 Freshman (Name "Judith" "Butler")
        ,Student 4 Senior (Name "Guy" "Debord")
        ,Student 5 Sophmore (Name "Jean" "Baudrillard")
        ,Student 6 Junior (Name "Julia" "Kristeva")]

-- creating database-like function select, where
_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

-- we can use lambda to sleect multiple properties like this
-- _select (\x -> (studentName x, gradeLevel x)) students

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where pred vals = do
    val <- vals
    guard (pred val)
    return val

-- helper function - checks whether or not word starts with some specific letter
startsWith :: Char -> String -> Bool
startsWith c word = c == head word

-- join function
-- joining Course and Teachers data types

data Teacher = Teacher {
        teacherId :: Int
        ,teacherName :: Name } deriving Show

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
            ,Teacher 200 (Name "Susan" "Sontag")]

data Course = Course {
        courseId :: Int
        ,courseTitle :: String
        ,teacher :: Int } deriving Show

courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 102 "English" 200]

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard (prop1 (fst dpairs) == prop2 (snd dpairs))
    return dpairs

-- building HINQ interface
_hinq selectQuery joinQuery whereQuery = (\joinData ->
                                            (\whereResult ->
                                                selectQuery whereResult)
                                            (whereQuery joinData)
                                         ) joinQuery

finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst))
                    (_join teachers courses teacherId teacher)
                    (_where ((=="English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))

-- now we making generic HINQ type
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause
                                                (_where (\_ -> True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where ((=="English") . courseTitle. snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

-- refactored select where and join can work with monads so
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                  (_join possibleTeacher possibleCourse teacherId teacher)
                  (_where ((=="French") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe Teacher Name
maybeQuery2 = HINQ_ (_select teacherName)
                    possibleTeacher

-- quering our data to determine course enrolment
data Enrollment = Enrollment
                    {student :: Int
                    ,course :: Int} deriving Show

enrollments :: [Enrollment]
enrollments = [Enrollment 1 101
              ,Enrollment 2 101
              ,Enrollment 2 201
              ,Enrollment 3 101
              ,Enrollment 4 201
              ,Enrollment 4 101
              ,Enrollment 5 101
              ,Enrollment 6 201 ]

studentEnrollmentsQ = HINQ_ (_select (\(st, en) -> (studentName st, course en)))
                           (_join students enrollments studentId student)

studentEnrolments :: [(Name, Int)]
studentEnrolments = runHINQ studentEnrollmentsQ

englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ = HINQ (_select (fst . fst))
                        (_join studentEnrolments courses snd courseId)
                        (_where ((=="English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ 

-- generic function to get enrollments
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
            where courseQuery = HINQ (_select (fst . fst))
                                     (_join studentEnrolments courses snd courseId)
                                     (_where ((== courseName) . courseTitle . snd))

