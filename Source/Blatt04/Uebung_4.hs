module Uebung_4 where
import Data.List
import Data.Function
import Data.List
import Data.Ord (comparing)
import Data.List (sortOn,sortBy)


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


averageGrade :: [Float] -> [Int] -> Float
averageGrade a b = sum $ zipWith (*) a $ map fromIntegral(b)



studentperformance :: [String] -> [[Int]] -> [Float] -> [(String, Float)]
studentperformance n g w = zip n $ map (\x-> averageGrade w x) g  


overallGrades :: [(String, Float)] -> [Float]
overallGrades t = map snd t 


overallAverage :: [(String, Float)] -> Float
overallAverage k = (sum $ map snd k) / fromIntegral(length k)


bestStudents :: [(String, Float)] -> [(String, Float)]
bestStudents x = [head $ sortBy (compare `on` snd) x]


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
