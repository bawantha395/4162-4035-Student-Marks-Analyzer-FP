# Student Marks Analyzer
**Functional Programming Mini Project**

## Group Members
- Member 1: [R.M.B.T.M. Rathnayake] - [EG/2020/4162]
- Member 2: [D.P.S.T. Kumari] - [EG/2020/4035]

## Project Description

### Real-World Scenario
Educational institutions process thousands of student records each year. Teachers and administrators need to:
- Calculate grades and rankings automatically
- Identify students needing academic support
- Generate statistical reports for accreditation
- Ensure data accuracy and auditability

This **Student Marks Analyzer** implements an ETL-style (Extract, Transform, Load) functional data pipeline that demonstrates how pure functional programming ensures:
- **Correctness**: Pure functions guarantee consistent outputs
- **Reliability**: Immutable data prevents accidental modifications
- **Maintainability**: Modular design enables easy testing and extension
- **Auditability**: All transformations are traceable and reproducible

## Features
✅ Parse student data with **subject names** from CSV format  
✅ Calculate averages, grades, and rankings  
✅ **Subject-wise performance analysis** for each student  
✅ **Subject-wise statistics** across all students  
✅ Identify strongest and weakest subjects per student  
✅ Find subject toppers  
✅ Generate comprehensive statistical analysis  
✅ Identify top performers and at-risk students  
✅ Grade distribution visualization  
✅ Search functionality by student ID  
✅ Detailed performance reports  
✅ Sample data included for quick testing  

## Installation & Setup

### Prerequisites
- GHC (Glasgow Haskell Compiler) version 8.10 or higher
- GHCi (Interactive environment)

### Installation
```bash
# Install GHC (if not installed)
# On Ubuntu/Debian:
sudo apt-get install ghc

# On macOS:
brew install ghc

# On Windows (recommended):
# Use `ghcup` to install GHC, GHCi and cabal. See https://www.haskell.org/ghcup/
# Alternatively download installers from the Haskell website.
```

## How to Run

### Method 1: Using GHCi (Recommended)
```bash
# Navigate to the project directory (the folder containing `Main.hs`)
cd path/to/project-directory

# Load the main module
ghci Main.hs

# Run the program
main
```

### Method 2: Compile and Run
```bash
# Compile the program
ghc -o analyzer Main.hs

# Run the executable
# On macOS/Linux:
./analyzer
# On Windows (PowerShell/Cmd):
.\analyzer.exe
analyzer.exe
```

## Usage Instructions

### Option 1: Enter Data Manually
1. Run the program
2. Select option `1` (Enter student data manually)
3. Enter number of students
4. Input details for each student:
   - Student ID (e.g., S001)
   - Student Name (e.g., John Doe)
   - Subjects with marks in format: `Subject1:Mark1,Subject2:Mark2,...`
   - Example: `Mathematics:85,Physics:90,Chemistry:78,Biology:92,English:88`

### Option 2: Load from File
1. Create a CSV file (e.g., `students.csv`) with format:
   ```
   S001,Alice Johnson,Mathematics:85,Physics:90,Chemistry:78,Biology:92,English:88
   S002,Bob Smith,Mathematics:72,Physics:68,Chemistry:75,Biology:70,English:73
   S003,Charlie Brown,Mathematics:95,Physics:92,Chemistry:98,Biology:94,English:96
   ```
   **Format:** Each line contains: `StudentID,Name,Subject1:Mark1,Subject2:Mark2,...`
2. Run the program
3. Select option `2` (Load from file)
4. Enter filename (e.g., `students.csv`)

### Web UI (Static preview)
If you'd like to preview the browser-based UI (`UI.html`) included with this repository, you can serve the project folder with Python's simple HTTP server and open the page in your browser.

Run this from the project root (where `UI.html` and `students.csv` live):

```bash
# Start a simple HTTP server on port 8000
python -m http.server 8000
```

Or on some Windows setups where `python` maps to Python 2, use:

```powershell
py -3 -m http.server 8000
```

Then open the UI in your browser:

http://localhost:8000/UI.html

Tip: Press Ctrl+C in the terminal to stop the server.

## Sample Input/Output

### Sample Input (CSV Format with Subjects)
```
S001,Alice Johnson,Mathematics:85,Physics:90,Chemistry:78,Biology:92,English:88
S002,Bob Smith,Mathematics:72,Physics:68,Chemistry:75,Biology:70,English:73
S003,Charlie Brown,Mathematics:95,Physics:92,Chemistry:98,Biology:94,English:96
S004,Diana Prince,Mathematics:88,Physics:85,Chemistry:90,Biology:87,English:89
S005,Eve Wilson,Mathematics:45,Physics:50,Chemistry:48,Biology:42,English:46
S006,Frank Miller,Mathematics:65,Physics:70,Chemistry:68,Biology:72,English:66
S007,Grace Lee,Mathematics:78,Physics:82,Chemistry:80,Biology:85,English:79
S008,Henry Davis,Mathematics:92,Physics:88,Chemistry:90,Biology:91,English:89
```
**Format:** `StudentID,StudentName,Subject1:Mark1,Subject2:Mark2,...`

### Sample Output
```
========== STUDENT REPORTS ==========
────────────────────────────────────────────────────────────
Student ID: S003
Name: Charlie Brown
Overall Average: 95.00
Overall Grade: A
Class Rank: 1

Subject-wise Performance:
  • Mathematics              Mark:  95.00  Grade: A
  • Physics                  Mark:  92.00  Grade: A
  • Chemistry                Mark:  98.00  Grade: A
  • Biology                  Mark:  94.00  Grade: A
  • English                  Mark:  96.00  Grade: A
────────────────────────────────────────────────────────────

========== CLASS STATISTICS ==========
Total Students:      8
Class Average:       78.40%
Highest Average:     95.00%
Lowest Average:      46.20%
════════════════════════════════════════════════════════════

========== SUBJECT-WISE STATISTICS ==========
Subject                  Average   Highest    Lowest
────────────────────────────────────────────────────────────
Biology                    79.00     94.00     42.00
Chemistry                  78.88     98.00     48.00
English                    79.00     96.00     46.00
Mathematics                79.75     95.00     45.00
Physics                    81.13     92.00     50.00
════════════════════════════════════════════════════════════

========== GRADE DISTRIBUTION ==========
Grade A:   2 students ████
Grade B:   2 students ████
Grade C:   0 students 
Grade S:   1 students ██
Grade F:   1 students ██
════════════════════════════════════════════════════════════
```

## Functional Programming Concepts Used

### 1. **Pure Functions**
All core logic functions have no side effects and always produce the same output for the same input.

```haskell
-- Example: Calculating average (pure function)
calculateAverage :: [Double] -> Double
calculateAverage [] = 0.0
calculateAverage xs = sum xs / fromIntegral (length xs)
```

### 2. **Immutability**
Data structures cannot be modified after creation, preventing bugs from shared mutable state.

```haskell
-- Student and subjects are immutable
data Subject = Subject
    { subjectName :: String
    , subjectMark :: Double
    } deriving (Show, Eq)

data Student = Student
    { studentId :: String
    , studentName :: String
    , subjects :: [Subject]
    } deriving (Show, Eq)
```

### 3. **Recursion**
Used instead of loops for iteration, demonstrating functional approach to repetition.

```haskell
-- Recursive maximum finding
findMax :: (Ord a) => [a] -> Maybe a
findMax [] = Nothing
findMax [x] = Just x
findMax (x:xs) = case findMax xs of
    Nothing -> Just x
    Just maxRest -> Just (max x maxRest)
```

### 4. **Higher-Order Functions**
Functions that take other functions as parameters or return functions.

```haskell
-- Filter using a predicate function
filterByGrade :: Grade -> [StudentReport] -> [StudentReport]
filterByGrade targetGrade = filter (\r -> grade r == targetGrade)

-- Map transformation (update subject marks)
transformMarks :: (Double -> Double) -> Student -> Student
transformMarks f student = student { subjects = map update (subjects student) }
    where update s = s { subjectMark = f (subjectMark s) }
```

### 5. **Algebraic Data Types (ADTs)**
Custom types that model domain concepts precisely.

```haskell
-- Grade as ADT
data Grade = A | B | C | S | F deriving (Show, Eq, Ord)

-- Result type for error handling
data Result a = Success a | Error String
```

### 6. **Pattern Matching**
Elegant way to handle different cases of data structures.

```haskell
determineGrade :: Double -> Grade
determineGrade avg
    | avg >= 75 = A
    | avg >= 65 = B
    | avg >= 55 = C
    | avg >= 35 = S
    | otherwise = F
```

### 7. **Function Composition**
Combining simple functions to create complex operations.

```haskell
-- Composing sort and take for top N students
getTopStudents :: Int -> [StudentReport] -> [StudentReport]
getTopStudents n = take n . sortByAverage
```

### 8. **Functors, Applicatives, and Monads**
Abstractions for handling computations with context (like error handling).

```haskell
-- Monadic error handling
processStudents :: [Student] -> Result [StudentReport]
processStudents students = 
    case mapM createStudentReport students of
        Success reports -> Success (assignRanks (sortByAverage reports))
        Error e -> Error e
```

### 9. **Lazy Evaluation**
Computations are deferred until results are needed, improving efficiency.

### 10. **Type Safety**
Strong static typing catches errors at compile time rather than runtime.

```haskell
-- Type signatures ensure correctness
calculateClassStatistics :: [StudentReport] -> Result Statistics
```

## File Structure
```
project root/
│
├── Main.hs              # Entry point, IO handling, menu system
├── DataTypes.hs         # Custom data type definitions
├── Processing.hs        # Core business logic (pure functions)
├── IOHandler.hs         # Input/output operations
├── Utils.hs             # Utility functions (calculations, sorting)
├── README.md            # This file
└── students.csv         # Sample data file (optional)
```

## Testing the Code

### Interactive Testing in GHCi
```haskell
-- Load modules
:load Main

-- Test individual functions
let student = Student "S001" "John" [Subject "Mathematics" 85, Subject "Physics" 90, Subject "Chemistry" 78]
calculateAverage (getMarks student)  -- Returns: 84.33...

-- Test grade determination
determineGrade 85  -- Returns: A

-- Test with sample data
let Right students = parseStudents sampleData
let Right reports = processStudents students
length reports  -- Shows number of students
```

## Why Functional Programming?

### Advantages Demonstrated in This Project:

1. **Correctness**: Pure functions make testing easier - same input always produces same output
2. **Reliability**: Immutable data prevents accidental modifications
3. **Maintainability**: Small, focused functions are easy to understand and modify
4. **Composability**: Complex operations built from simple, reusable pieces
5. **Concurrency-Ready**: Pure functions can be safely parallelized (future extension)
6. **Type Safety**: Compiler catches many errors before runtime

## Possible Extensions

- 📊 Export reports to PDF/Excel
- 📈 Add data visualization (charts/graphs)
- 🔄 Support multiple semesters/courses
- 🌐 Web interface using Servant/Yesod
- ⚡ Parallel processing for large datasets using `parallel` library
- 💾 Database integration (PostgreSQL with Hasql)
- 📧 Email notifications for at-risk students
- 🎯 Predictive analytics using machine learning

## Dependencies
- Base Haskell libraries (no external packages required)
- GHC 8.10+

## License
MIT License - Educational Project

## Contact
For questions or issues, please contact group members.

---
*This project demonstrates core functional programming principles through a practical, real-world application in educational data processing.*