-- Main.hs
-- Entry point for the Student Marks Analyzer application

module Main where

import DataTypes
import Processing
import IOHandler
import Utils
import System.IO
import Control.Monad (when)

-- | Main menu display
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n================================ STUDENT MARKS ANALYZER ================================="
    putStrLn "                           (With Subject-wise Analysis)"
    putStrLn "======================================================================================"
    putStrLn "1. Enter student data manually"
    putStrLn "2. Load from file"
    putStrLn "3. Exit"
    putStr "Enter your choice: "
    hFlush stdout

-- | Analysis menu after data is loaded
displayAnalysisMenu :: IO ()
displayAnalysisMenu = do
    putStrLn "\n============================= ANALYSIS OPTIONS ============================="
    putStrLn "1. View all student reports"
    putStrLn "2. View class statistics"
    putStrLn "3. View subject-wise statistics"
    putStrLn "4. View grade distribution"
    putStrLn "5. View top 3 students"
    putStrLn "6. View students needing attention"
    putStrLn "7. Search student by ID"
    putStrLn "8. View detailed student performance"
    putStrLn "9. Find subject topper"
    putStrLn "10. Back to main menu"
    putStr "Enter your choice: "
    hFlush stdout

-- | Process and analyze student data
analyzeData :: String -> IO ()
analyzeData csvData = do
    case parseStudents csvData of
        Error err -> displayError err
        Success students -> do
            putStrLn $ "\nSuccessfully loaded " ++ show (length students) ++ " students."
            case processStudents students of
                Error err -> displayError err
                Success reports -> do
                    case calculateClassStatistics reports of
                        Error err -> displayError err
                        Success stats -> analysisLoop reports stats students

-- | Analysis loop
analysisLoop :: [StudentReport] -> Statistics -> [Student] -> IO ()
analysisLoop reports stats students = do
    displayAnalysisMenu
    choice <- getLine
    case choice of
        "1" -> do
            displayAllReports reports
            analysisLoop reports stats students
        "2" -> do
            displayStatistics stats
            analysisLoop reports stats students
        "3" -> do
            let subjStats = calculateSubjectStatistics students
            displaySubjectStatistics subjStats
            analysisLoop reports stats students
        "4" -> do
            let dist = calculateGradeDistribution reports
            displayGradeDistribution dist
            analysisLoop reports stats students
        "5" -> do
            displayTopStudents 3 reports
            analysisLoop reports stats students
        "6" -> do
            displayFailedStudents reports
            analysisLoop reports stats students
        "7" -> do
            searchStudent reports
            analysisLoop reports stats students
        "8" -> do
            viewDetailedPerformance reports
            analysisLoop reports stats students
        "9" -> do
            findSubjectTopperMenu students
            analysisLoop reports stats students
        "10" -> mainLoop
        _ -> do
            putStrLn "Invalid choice. Please try again."
            analysisLoop reports stats students

-- | Search for a student by ID
searchStudent :: [StudentReport] -> IO ()
searchStudent reports = do
    putStr "\nEnter student ID: "
    hFlush stdout
    sid <- getLine
    let found = filter (\r -> studentId (reportStudent r) == sid) reports
    case found of
        [] -> putStrLn $ "No student found with ID: " ++ sid
        (report:_) -> displayStudentReport report

-- | View detailed performance for a student
viewDetailedPerformance :: [StudentReport] -> IO ()
viewDetailedPerformance reports = do
    putStr "\nEnter student ID for detailed report: "
    hFlush stdout
    sid <- getLine
    let found = filter (\r -> studentId (reportStudent r) == sid) reports
    case found of
        [] -> putStrLn $ "No student found with ID: " ++ sid
        (report:_) -> displayDetailedPerformance report

-- | Find topper in a specific subject
findSubjectTopperMenu :: [Student] -> IO ()
findSubjectTopperMenu students = do
    putStr "\nEnter subject name: "
    hFlush stdout
    subj <- getLine
    case getSubjectTopper subj students of
        Nothing -> putStrLn $ "No data found for subject: " ++ subj
        Just (student, mark) -> do
            putStrLn $ "\nSubject Topper in " ++ subj ++ ":"
            putStrLn $ "Student: " ++ studentName student
            putStrLn $ "ID: " ++ studentId student
            putStrLn $ "Mark: " ++ show mark

-- | Read student data manually
readStudentManually :: IO String
readStudentManually = do
    putStr "Enter number of students: "
    hFlush stdout
    nStr <- getLine
    case reads nStr of
        [(n, "")] | n > 0 -> readNStudents n ""
        _ -> do
            putStrLn "Invalid number. Please try again."
            readStudentManually

-- | Read N students data
readNStudents :: Int -> String -> IO String
readNStudents 0 acc = return acc
readNStudents n acc = do
    putStrLn $ "\n=== Enter details for student " ++ show (n) ++ " ==="
    putStr "Student ID: "
    hFlush stdout
    sid <- getLine
    putStr "Student Name: "
    hFlush stdout
    name <- getLine
    putStrLn "Enter subjects (format: SubjectName:Mark)"
    putStrLn "Example: Mathematics:85,Physics:90,Chemistry:78"
    putStr "Subjects: "
    hFlush stdout
    subjectsStr <- getLine
    let studentLine = sid ++ "," ++ name ++ "," ++ subjectsStr
    readNStudents (n - 1) (acc ++ studentLine ++ "\n")

-- | Read data from file
readFromFile :: IO String
readFromFile = do
    putStr "Enter filename (with .csv extension): "
    hFlush stdout
    filename <- getLine
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    -- Force evaluation before closing handle
    let len = length contents
    len `seq` hClose handle
    return contents

-- | Main loop
mainLoop :: IO ()
mainLoop = do
    displayMenu
    choice <- getLine
    case choice of
        "1" -> do
            csvData <- readStudentManually
            analyzeData csvData
        "2" -> do
            result <- readFromFile
            analyzeData result
        "3" -> do
            putStrLn "\n═══════════════════════════════════════════════════════"
            putStrLn "  Thank you for using Student Marks Analyzer!"
            putStrLn "  Goodbye!"
            putStrLn "═══════════════════════════════════════════════════════"
        _ -> do
            putStrLn "Invalid choice. Please try again."
            mainLoop

-- | Application entry point
main :: IO ()
main = do
    putStrLn "═══════════════════════════════════════════════════════════"
    putStrLn "          Student Marks Analyzer"
    putStrLn "      Functional Programming Mini Project"
    putStrLn "      (With Subject-wise Performance Tracking)"
    putStrLn "═══════════════════════════════════════════════════════════"
    mainLoop