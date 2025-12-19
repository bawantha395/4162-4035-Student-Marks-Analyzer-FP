-- IOHandler.hs
-- Handles all IO operations (separated from pure logic)

module IOHandler where

import DataTypes
import Processing
import Utils
import Control.Monad (forM_)

-- | Parse a CSV line into a Student record with subject names
-- Format: StudentID,Name,Subject1:Mark1,Subject2:Mark2,...
parseStudentLine :: String -> Result Student
parseStudentLine line = 
    let parts = splitOn ',' line
    in case parts of
        (sid:name:subjectData) -> 
            case mapM parseSubject subjectData of
                Just subs -> Success $ Student sid name subs
                Nothing -> Error $ "Invalid subject format in line: " ++ line
        _ -> Error $ "Invalid line format: " ++ line

-- | Parse subject in format "SubjectName:Mark"
parseSubject :: String -> Maybe Subject
parseSubject str = 
    let parts = splitOn ':' str
    in case parts of
        [name, markStr] -> 
            case parseDouble markStr of
                Just mark -> Just $ Subject (trim name) mark
                Nothing -> Nothing
        _ -> Nothing

-- | Split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delim cs

-- | Parse string to Double
parseDouble :: String -> Maybe Double
parseDouble s = case reads (trim s) of
    [(val, "")] -> Just val
    _ -> Nothing

-- | Trim whitespace from string
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\n\r")

-- | Parse multiple student lines
parseStudents :: String -> Result [Student]
parseStudents content = 
    let lns = lines content
        nonEmptyLines = filter (not . null . trim) lns
    in mapM parseStudentLine nonEmptyLines

-- | Display a single student report
displayStudentReport :: StudentReport -> IO ()
displayStudentReport report = do
    let student = reportStudent report
    putStrLn $ replicate 60 '─'
    putStrLn $ "Student ID: " ++ studentId student
    putStrLn $ "Name: " ++ studentName student
    putStrLn ""
    putStrLn "📚 SUBJECTS AND MARKS:"
    -- Display each subject with its name and mark
    forM_ (subjects student) $ \subject ->
        putStrLn $ "  • " ++ padRight 20 (subjectName subject) ++ 
                   " → " ++ printf "%.2f" (subjectMark subject)
    putStrLn ""
    putStrLn $ "Overall Average: " ++ printf "%.2f" (avgMark report) ++ "%"
    putStrLn $ "Overall Grade: " ++ show (grade report)
    putStrLn $ "Class Rank: " ++ show (rank report)
    putStrLn ""
    putStrLn "Subject-wise Grades:"
    forM_ (subjectWisePerformance report) $ \(subj, mark, g) ->
        putStrLn $ "  • " ++ padRight 20 subj ++ 
                   " Mark: " ++ padLeft 6 (printf "%.2f" mark) ++ 
                   "  Grade: " ++ show g

-- | Pad string to right
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- | Pad string to left
padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | Format double to 2 decimal places (pure)
printf :: String -> Double -> String
printf "%.2f" d = show (fromIntegral (round (d * 100)) / 100 :: Double)
printf _ d = show d

-- | Display all student reports
displayAllReports :: [StudentReport] -> IO ()
displayAllReports reports = do
    putStrLn "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn "║              COMPLETE STUDENT REPORTS                      ║"
    putStrLn "╚════════════════════════════════════════════════════════════╝"
    forM_ reports displayStudentReport
    putStrLn $ replicate 60 '═'

-- | Display class statistics
displayStatistics :: Statistics -> IO ()
displayStatistics stats = do
    putStrLn "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn "║                CLASS STATISTICS                            ║"
    putStrLn "╚════════════════════════════════════════════════════════════╝"
    putStrLn $ "Total Students:      " ++ show (totalStudents stats)
    putStrLn $ "Class Average:       " ++ printf "%.2f" (average stats) ++ "%"
    putStrLn $ "Highest Average:     " ++ printf "%.2f" (highest stats) ++ "%"
    putStrLn $ "Lowest Average:      " ++ printf "%.2f" (lowest stats) ++ "%"
    putStrLn $ replicate 60 '═'

-- | Display subject-wise statistics
displaySubjectStatistics :: [SubjectStatistics] -> IO ()
displaySubjectStatistics stats = do
    putStrLn "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn "║            SUBJECT-WISE STATISTICS                         ║"
    putStrLn "╚════════════════════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn $ padRight 25 "Subject" ++ padLeft 10 "Average" ++ 
               padLeft 10 "Highest" ++ padLeft 10 "Lowest"
    putStrLn $ replicate 60 '─'
    forM_ stats $ \s -> do
        putStrLn $ padRight 25 (statSubjectName s) ++ 
                   padLeft 10 (printf "%.2f" (subjectAverage s)) ++
                   padLeft 10 (printf "%.2f" (subjectHighest s)) ++
                   padLeft 10 (printf "%.2f" (subjectLowest s))
    putStrLn $ replicate 60 '═'

-- | Display grade distribution
displayGradeDistribution :: [(Grade, Int)] -> IO ()
displayGradeDistribution dist = do
    putStrLn "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn "║              GRADE DISTRIBUTION                            ║"
    putStrLn "╚════════════════════════════════════════════════════════════╝"
    forM_ dist $ \(g, count) -> do
        let bar = replicate (count * 2) '█'
        putStrLn $ "Grade " ++ show g ++ ": " ++ 
                   padLeft 3 (show count) ++ " students " ++ bar
    putStrLn $ replicate 60 '═'

-- | Display top students
displayTopStudents :: Int -> [StudentReport] -> IO ()
displayTopStudents n reports = do
    putStrLn $ "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn $ "║              TOP " ++ show n ++ " STUDENTS                                  ║"
    putStrLn $ "╚════════════════════════════════════════════════════════════╝"
    let topN = getTopStudents n reports
    forM_ topN $ \report -> do
        let student = reportStudent report
        putStrLn $ padLeft 3 (show (rank report)) ++ ". " ++ 
                   padRight 30 (studentName student) ++ 
                   " Average: " ++ printf "%.2f" (avgMark report) ++
                   " (Grade: " ++ show (grade report) ++ ")"
    putStrLn $ replicate 60 '═'

-- | Display students who failed
displayFailedStudents :: [StudentReport] -> IO ()
displayFailedStudents reports = do
    let failed = getFailedStudents reports
    putStrLn $ "\n╔════════════════════════════════════════════════════════════╗"
    putStrLn $ "║        STUDENTS NEEDING ATTENTION (Grade F)                ║"
    putStrLn $ "╚════════════════════════════════════════════════════════════╝"
    if null failed
        then putStrLn "✓ Excellent! No students failed."
        else forM_ failed $ \report -> do
            let student = reportStudent report
            putStrLn $ "⚠ " ++ padRight 30 (studentName student) ++ 
                       " ID: " ++ padRight 8 (studentId student) ++ 
                       " Avg: " ++ printf "%.2f" (avgMark report)
    putStrLn $ replicate 60 '═'

-- | Print error message
displayError :: String -> IO ()
displayError err = putStrLn $ "❌ ERROR: " ++ err

-- | Sample data for testing with subjects
sampleData :: String
sampleData = unlines
    [ "S001,Alice Johnson,Mathematics:85,Physics:90,Chemistry:78,Biology:92,English:88"
    , "S002,Bob Smith,Mathematics:72,Physics:68,Chemistry:75,Biology:70,English:73"
    , "S003,Charlie Brown,Mathematics:95,Physics:92,Chemistry:98,Biology:94,English:96"
    , "S004,Diana Prince,Mathematics:88,Physics:85,Chemistry:90,Biology:87,English:89"
    , "S005,Eve Wilson,Mathematics:45,Physics:50,Chemistry:48,Biology:42,English:46"
    , "S006,Frank Miller,Mathematics:65,Physics:70,Chemistry:68,Biology:72,English:66"
    , "S007,Grace Lee,Mathematics:78,Physics:82,Chemistry:80,Biology:85,English:79"
    , "S008,Henry Davis,Mathematics:92,Physics:88,Chemistry:90,Biology:91,English:89"
    ]

-- | Display detailed performance for a specific student
displayDetailedPerformance :: StudentReport -> IO ()
displayDetailedPerformance report = do
    putStrLn $ generatePerformanceSummary report