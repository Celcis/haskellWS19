module Uebung_4 where
import Data.List
import Data.Function

-- 4.1
concat' :: [[a]] -> [a]
concat' = foldl (++) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x c -> if p x then x : c else c) []

-- 4.2

grades :: [[Int]]
grades = [[6, 4], [1, 3], [5, 2], [3, 1]]

weights :: [Float]
weights = [0.75, 0.25]

names = ["Daniel", "Mahmoud", "Marcel", "Fawad"]

--(*) = (*) `on` fromIntegral


averageGrade :: [Float] -> [Int] -> Float
averageGrade a b = sum $ zipWith (*) a $ map fromIntegral(b)



studentperformance :: [String] -> [[Int]] -> [Float] -> [(String, Float)]
studentperformance  = undefined

overallGrades :: [(String, Float)] -> [Float]
overallGrades = undefined

overallAverage :: [(String, Float)] -> Float
overallAverage = undefined

bestStudents :: [(String, Float)] -> [(String, Float)]
bestStudents = undefined

getPartialList :: ((String, Float) -> Bool) -> [(String, Float)] -> [(String, Float)]
getPartialList = undefined

successRate :: [(String, Float)] -> Float
successRate = undefined

hasPassed :: (String, Float) -> Bool
hasPassed (_, avg) = avg <= 4

whichPassed :: [(String, Float)]
whichPassed = undefined

whichAreAboveOverallAverage :: [(String, Float)]
whichAreAboveOverallAverage = undefined

allPassed :: [(String, Float)] -> Bool
allPassed = undefined
