-- Utils.hs
-- Utility functions for calculations and transformations

module Utils where

import DataTypes

-- | Extract all marks from a student's subjects
getMarks :: Student -> [Double]
getMarks = map subjectMark . subjects

-- | Calculate average of a list of numbers (pure function)
calculateAverage :: [Double] -> Double
calculateAverage [] = 0.0
calculateAverage xs = sum xs / fromIntegral (length xs)

-- | Determine grade based on average marks
determineGrade :: Double -> Grade
determineGrade avg
    | avg >= 75 = A
    | avg >= 65 = B
    | avg >= 55 = C
    | avg >= 35 = S
    | otherwise = F

-- | Find maximum value in a list (recursive implementation)
findMax :: (Ord a) => [a] -> Maybe a
findMax [] = Nothing
findMax [x] = Just x
findMax (x:xs) = case findMax xs of
    Nothing -> Just x
    Just maxRest -> Just (max x maxRest)

-- | Find minimum value in a list (recursive implementation)
findMin :: (Ord a) => [a] -> Maybe a
findMin [] = Nothing
findMin [x] = Just x
findMin (x:xs) = case findMin xs of
    Nothing -> Just x
    Just minRest -> Just (min x minRest)

-- | Sort students by average marks (descending) - Pure merge sort
sortByAverage :: [StudentReport] -> [StudentReport]
sortByAverage = mergeSort compareByAvg
  where
    compareByAvg r1 r2 = avgMark r1 >= avgMark r2

-- | Generic merge sort implementation (higher-order function)
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort comp xs = merge comp (mergeSort comp left) (mergeSort comp right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

-- | Merge two sorted lists
merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge comp (x:xs) (y:ys)
    | comp x y = x : merge comp xs (y:ys)
    | otherwise = y : merge comp (x:xs) ys

-- | Assign ranks to sorted student reports
assignRanks :: [StudentReport] -> [StudentReport]
assignRanks reports = zipWith setRank [1..] reports
  where
    setRank r report = report { rank = r }

-- | Filter students by grade (higher-order function)
filterByGrade :: Grade -> [StudentReport] -> [StudentReport]
filterByGrade targetGrade = filter (\r -> grade r == targetGrade)

-- | Count students by grade (using fold)
countByGrade :: Grade -> [StudentReport] -> Int
countByGrade targetGrade = foldr (\r acc -> if grade r == targetGrade then acc + 1 else acc) 0

-- | Validate marks (all marks should be between 0 and 100)
validateMarks :: [Double] -> Result [Double]
validateMarks marks
    | null marks = Error "Marks list cannot be empty"
    | any (\m -> m < 0 || m > 100) marks = Error "Marks must be between 0 and 100"
    | otherwise = Success marks

-- | Validate subject (must have name and valid mark)
validateSubject :: Subject -> Result Subject
validateSubject subject
    | null (subjectName subject) = Error "Subject name cannot be empty"
    | subjectMark subject < 0 || subjectMark subject > 100 = 
        Error $ "Invalid mark for " ++ subjectName subject ++ ": must be between 0 and 100"
    | otherwise = Success subject

-- | Safe division
safeDivide :: Double -> Double -> Result Double
safeDivide _ 0 = Error "Division by zero"
safeDivide x y = Success (x / y)

-- | Calculate percentage
calculatePercentage :: Double -> Double -> Result Double
calculatePercentage obtained total = 
    safeDivide obtained total >>= \result -> Success (result * 100)

-- | Get subject-wise performance for a student
getSubjectPerformance :: Student -> [(String, Double, Grade)]
getSubjectPerformance student = 
    map (\s -> (subjectName s, subjectMark s, determineGrade (subjectMark s))) 
        (subjects student)

-- | Find student's weakest subject (lowest mark)
findWeakestSubject :: Student -> Maybe Subject
findWeakestSubject student = 
    case subjects student of
        [] -> Nothing
        subs -> Just $ foldr1 (\s1 s2 -> if subjectMark s1 < subjectMark s2 then s1 else s2) subs

-- | Find student's strongest subject (highest mark)
findStrongestSubject :: Student -> Maybe Subject
findStrongestSubject student = 
    case subjects student of
        [] -> Nothing
        subs -> Just $ foldr1 (\s1 s2 -> if subjectMark s1 > subjectMark s2 then s1 else s2) subs