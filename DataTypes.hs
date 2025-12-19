-- DataTypes.hs
-- Defines all custom data types used in the Student Marks Analyzer

module DataTypes where

-- | Represents a subject with name and marks
data Subject = Subject
    { subjectName :: String
    , subjectMark :: Double
    } deriving (Show, Eq)

-- | Represents a student with their information
data Student = Student
    { studentId :: String
    , studentName :: String
    , subjects :: [Subject]  -- List of subjects with marks
    } deriving (Show, Eq)

-- | Grade classification based on average marks
data Grade = A | B | C | D | F deriving (Show, Eq, Ord)

-- | Statistical summary for analysis
data Statistics = Statistics
    { average :: Double
    , highest :: Double
    , lowest :: Double
    , totalStudents :: Int
    } deriving (Show, Eq)

-- | Complete student report with calculated values
data StudentReport = StudentReport
    { reportStudent :: Student
    , avgMark :: Double
    , grade :: Grade
    , rank :: Int
    , subjectWisePerformance :: [(String, Double, Grade)]  -- Subject, Mark, Grade
    } deriving (Show, Eq)

-- | Subject-wise statistics across all students
data SubjectStatistics = SubjectStatistics
    { statSubjectName :: String
    , subjectAverage :: Double
    , subjectHighest :: Double
    , subjectLowest :: Double
    } deriving (Show, Eq)

-- | Result type for error handling (pure functional approach)
data Result a = Success a | Error String deriving (Show, Eq)

-- Functor instance for Result (enables fmap)
instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error e) = Error e

-- Applicative instance for Result
instance Applicative Result where
    pure = Success
    Success f <*> Success a = Success (f a)
    Error e <*> _ = Error e
    _ <*> Error e = Error e

-- Monad instance for Result (enables do-notation)
instance Monad Result where
    Success a >>= f = f a
    Error e >>= _ = Error e