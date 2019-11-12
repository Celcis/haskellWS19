module Test where

import Uebung_4

import Test.Tasty
import Test.Tasty.HUnit

run = defaultMain allTests

allTests = testGroup "allTests" [tests]

tests = testGroup "Tests zu Uebungsblatt 4" 
  [ testCase "averageGrade of Daniel ist 5.5" $
    assertEqual "wrong result" 5.5 (averageGrade weights (grades!!0)),
    testCase "studentperformance " $
    assertEqual "wrong result" [("Daniel",5.5),("Mahmoud",1.5),("Marcel",4.25),("Fawad",2.5)] (studentperformance names grades weights),
    testCase "overallGrades" $
    assertEqual "wrong result" [5.5,1.5,4.25,2.5] (overallGrades $ studentperformance names grades weights) ,
    testCase "overallAverage" $ 
    assertEqual "wrong result" 3.4375 (overallAverage $ studentperformance names grades weights),
    testCase "bestStudents" $ 
    assertEqual "wrong result" [("Mahmoud",1.5)] (bestStudents $ studentperformance names grades weights),
    testCase "successRate" $ 
    assertEqual "wrong result" 0.5
 (successRate $ studentperformance names grades weights),
    testCase "whichPassed" $ 
    assertEqual "wrong result" [("Mahmoud",1.5),("Fawad",2.5)] (whichPassed),
    testCase "successRate" $ 
    assertEqual "wrong result" [("Mahmoud",1.5),("Fawad",2.5)] (whichAreAboveOverallAverage),
    testCase "allPassed" $ 
    assertEqual "wrong result" False (allPassed $ studentperformance names grades weights)
  ]
