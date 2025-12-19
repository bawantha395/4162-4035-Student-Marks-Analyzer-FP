-- Processing.hs
-- Core processing logic for student marks analysis

module Processing where

import DataTypes
import Utils
import Data.List (groupBy, sortBy)
import Data.Function (on)

-- | Create a student report from a student record (pure function)
createStudentReport :: Student -> Result StudentReport
createStudentReport student = do
    let marks = getMarks student
    validMarks <- validateMarks marks
    let avg = calculateAverage validMarks
    let studentGrade = determineGrade avg
    let subjPerf = getSubjectPerformance student
    Success $ StudentReport
        { reportStudent = student
        , avgMark = avg
        , grade = studentGrade
        , rank = 0  -- Will be assigned later
        , subjectWisePerformance = subjPerf
        }

-- | Process multiple students and generate reports
processStudents :: [Student] -> Result [StudentReport]
processStudents students = 
    case mapM createStudentReport students of
        Success reports -> Success (assignRanks (sortByAverage reports))
        Error e -> Error e

-- | Calculate class statistics from student reports
calculateClassStatistics :: [StudentReport] -> Result Statistics
calculateClassStatistics [] = Error "No students to analyze"
calculateClassStatistics reports = 
    let averages = map avgMark reports
        maxMark = findMax averages
        minMark = findMin averages
    in case (maxMark, minMark) of
        (Just mx, Just mn) -> Success $ Statistics
            { average = calculateAverage averages
            , highest = mx
            , lowest = mn
            , totalStudents = length reports
            }
        _ -> Error "Unable to calculate statistics"

-- | Calculate subject-wise statistics across all students
calculateSubjectStatistics :: [Student] -> [SubjectStatistics]
calculateSubjectStatistics students = 
    let allSubjects = concatMap subjects students
        groupedByName = groupBy ((==) `on` subjectName) $ 
                        sortBy (compare `on` subjectName) allSubjects
    in map calculateStatsForSubject groupedByName
  where
    calculateStatsForSubject :: [Subject] -> SubjectStatistics
    calculateStatsForSubject subs =
        let marks = map subjectMark subs
            name = subjectName (head subs)
        in SubjectStatistics
            { statSubjectName = name
            , subjectAverage = calculateAverage marks
            , subjectHighest = maybe 0 id (findMax marks)
            , subjectLowest = maybe 0 id (findMin marks)
            }

-- | Get top N students (higher-order function with partial application)
getTopStudents :: Int -> [StudentReport] -> [StudentReport]
getTopStudents n = take n . sortByAverage

-- | Get students who failed (mark < 50)
getFailedStudents :: [StudentReport] -> [StudentReport]
getFailedStudents = filter (\r -> grade r == F)

-- | Get students by grade range
getStudentsByGradeRange :: [Grade] -> [StudentReport] -> [StudentReport]
getStudentsByGradeRange grades reports = 
    filter (\r -> grade r `elem` grades) reports

-- | Calculate grade distribution (returns list of tuples)
calculateGradeDistribution :: [StudentReport] -> [(Grade, Int)]
calculateGradeDistribution reports = 
    map (\g -> (g, countByGrade g reports)) [A, B, C, D, F]

-- | Find students above average (demonstrates function composition)
getAboveAverageStudents :: [StudentReport] -> Statistics -> [StudentReport]
getAboveAverageStudents reports stats = 
    filter (\r -> avgMark r > average stats) reports

-- | Find students below average
getBelowAverageStudents :: [StudentReport] -> Statistics -> [StudentReport]
getBelowAverageStudents reports stats = 
    filter (\r -> avgMark r < average stats) reports

-- | Generate performance summary for a student
generatePerformanceSummary :: StudentReport -> String
generatePerformanceSummary report = 
    let student = reportStudent report
        performanceLevel = case grade report of
            A -> "Excellent"
            B -> "Good"
            C -> "Average"
            D -> "Below Average"
            F -> "Needs Improvement"
        weakest = findWeakestSubject student
        strongest = findStrongestSubject student
    in unlines $
        [ "╔══════════════════════════════════════════════════"
        , "║ STUDENT PERFORMANCE SUMMARY"
        , "╚══════════════════════════════════════════════════"
        , "Student: " ++ studentName student
        , "ID: " ++ studentId student
        , "Average Mark: " ++ show (avgMark report)
        , "Overall Grade: " ++ show (grade report)
        , "Class Rank: " ++ show (rank report)
        , "Performance Level: " ++ performanceLevel
        , ""
        , "Subject-wise Performance:"
        ] ++ formatSubjects (subjectWisePerformance report)
        ++ [ ""
           , case strongest of
               Just s -> "Strongest Subject: " ++ subjectName s ++ 
                        " (" ++ show (subjectMark s) ++ ")"
               Nothing -> "No subject data"
           , case weakest of
               Just s -> "Weakest Subject: " ++ subjectName s ++ 
                        " (" ++ show (subjectMark s) ++ ")"
               Nothing -> "No subject data"
           ]
  where
    formatSubjects :: [(String, Double, Grade)] -> [String]
    formatSubjects = map (\(name, mark, g) -> 
        "  • " ++ name ++ ": " ++ show mark ++ " (Grade: " ++ show g ++ ")")

-- | Apply a transformation to all subject marks (demonstrates map/functor)
transformMarks :: (Double -> Double) -> Student -> Student
transformMarks f student = 
    student { subjects = map (\s -> s { subjectMark = f (subjectMark s) }) (subjects student) }

-- | Add bonus marks to all subjects (partial application example)
addBonusMarks :: Double -> Student -> Student
addBonusMarks bonus = transformMarks (+ bonus)

-- | Apply curve to marks (scale marks)
applyCurve :: Double -> Student -> Student
applyCurve factor = transformMarks (* factor)

-- | Find students failing in specific subject
getStudentsFailingInSubject :: String -> [Student] -> [Student]
getStudentsFailingInSubject subjectName students = 
    filter hasFailingSubject students
  where
    hasFailingSubject student = 
        any (\s -> subjectName == DataTypes.subjectName s && subjectMark s < 50) 
            (subjects student)

-- | Get topper in a specific subject
getSubjectTopper :: String -> [Student] -> Maybe (Student, Double)
getSubjectTopper subjName students = 
    let studentsWithMark = [(s, mark) | s <- students, 
                                        sub <- subjects s, 
                                        DataTypes.subjectName sub == subjName,
                                        let mark = subjectMark sub]
    in case studentsWithMark of
        [] -> Nothing
        _ -> Just $ foldr1 (\(s1, m1) (s2, m2) -> if m1 > m2 then (s1, m1) else (s2, m2)) 
                           studentsWithMark